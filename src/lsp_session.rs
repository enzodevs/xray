//! Unix-only private worker lifecycle and bounded local JSON protocol.
use std::fs::{File, OpenOptions};
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::os::fd::AsRawFd;
use std::os::unix::fs::{DirBuilderExt, FileTypeExt, MetadataExt, OpenOptionsExt, PermissionsExt};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde::{de::DeserializeOwned, Deserialize, Serialize};

use super::{NavigationResult, Request, Workspace};
use crate::error::XrayError;
use crate::lsp::{
    failure, nonblocking, poll_descriptors, read_until, write_until, MAX_MESSAGE, TIMEOUT,
};

const QUERY_TIMEOUT: Duration = Duration::from_secs(210);
const IDLE_TIMEOUT: Duration = Duration::from_secs(120);

#[derive(Serialize, Deserialize)]
enum Reply {
    Success(NavigationResult),
    Failure(String),
}

struct Paths {
    directory: PathBuf,
    socket: PathBuf,
}

fn private_directory(path: &Path) -> Result<(), XrayError> {
    match std::fs::DirBuilder::new().mode(0o700).create(path) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(failure(format!("create {}: {e}", path.display()))),
    }
    let metadata = std::fs::symlink_metadata(path).map_err(|e| failure(e.to_string()))?;
    // SAFETY: geteuid has no arguments or memory access requirements.
    let uid = unsafe { libc::geteuid() };
    if !metadata.is_dir() || metadata.uid() != uid || metadata.mode() & 0o777 != 0o700 {
        return Err(failure(format!(
            "{} must be an owned, non-symlink 0700 directory",
            path.display()
        )));
    }
    Ok(())
}

fn paths(root: &Path) -> Result<Paths, XrayError> {
    // Fixed /tmp avoids trusting a project-controlled TMPDIR; only the owned 0700 subtree is used.
    let base = PathBuf::from(format!("/tmp/xray-lsp-{}", unsafe { libc::geteuid() }));
    private_directory(&base)?;
    let mut hash = std::collections::hash_map::DefaultHasher::new();
    root.hash(&mut hash);
    let exe = std::env::current_exe().map_err(|e| failure(e.to_string()))?;
    exe.hash(&mut hash);
    let metadata = exe.metadata().map_err(|e| failure(e.to_string()))?;
    metadata.len().hash(&mut hash);
    metadata
        .modified()
        .map_err(|e| failure(e.to_string()))?
        .hash(&mut hash);
    // Resolve symlinks and fingerprint the actual server, so package upgrades get a new session.
    let server = crate::lsp::php_executable()?;
    server.hash(&mut hash);
    let metadata = server.metadata().map_err(|e| failure(e.to_string()))?;
    metadata.len().hash(&mut hash);
    metadata
        .modified()
        .map_err(|e| failure(e.to_string()))?
        .hash(&mut hash);
    // Activation/replacement/removal must not reuse a worker's old capability negotiation.
    hash_licence(crate::lsp::php_licence_file()?.as_deref(), &mut hash)?;
    // Different explicitly selected installations must not silently share a server.
    std::env::var_os("XRAY_INTELEPHENSE").hash(&mut hash);
    std::env::var_os("PATH").hash(&mut hash);
    let directory = base.join(format!("{:016x}", hash.finish()));
    private_directory(&directory)?;
    Ok(Paths {
        socket: directory.join("session.sock"),
        directory,
    })
}

fn hash_licence(path: Option<&Path>, hash: &mut impl Hasher) -> Result<(), XrayError> {
    path.hash(hash);
    if let Some(path) = path {
        let metadata = path
            .metadata()
            .map_err(|e| failure(format!("licence metadata: {e}")))?;
        metadata.len().hash(hash);
        metadata
            .modified()
            .map_err(|e| failure(e.to_string()))?
            .hash(hash);
        metadata.ino().hash(hash);
        metadata.ctime().hash(hash);
        metadata.ctime_nsec().hash(hash);
    }
    Ok(())
}

fn lock(path: &Path, deadline: Instant, wait: bool) -> Result<File, XrayError> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(path)
        .map_err(|e| failure(format!("lock: {e}")))?;
    let metadata = file.metadata().map_err(|e| failure(e.to_string()))?;
    if !metadata.is_file()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o777 != 0o600
    {
        return Err(failure("unsafe worker lock file"));
    }
    loop {
        // SAFETY: flock only accesses this live file descriptor.
        if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) } == 0 {
            return Ok(file);
        }
        let error = std::io::Error::last_os_error();
        if error.kind() != std::io::ErrorKind::WouldBlock || !wait {
            return Err(failure(format!("worker lock: {error}")));
        }
        if Instant::now() >= deadline {
            return Err(failure("worker queue deadline exceeded"));
        }
        // This is lock contention backoff, not an LSP indexing/readiness heuristic.
        std::thread::sleep(Duration::from_millis(10));
    }
}

pub(super) fn query(root: &Path, request: &Request) -> Result<NavigationResult, XrayError> {
    let paths = paths(root)?;
    let deadline = Instant::now() + QUERY_TIMEOUT;
    let _queue = lock(&paths.directory.join("queue.lock"), deadline, true)?;
    let mut stream = match connect_socket(&paths.socket) {
        Ok(stream) => stream,
        Err(error)
            if matches!(
                error.kind(),
                std::io::ErrorKind::NotFound | std::io::ErrorKind::ConnectionRefused
            ) =>
        {
            launch(root, deadline)?;
            connect_socket(&paths.socket).map_err(|e| failure(format!("connect worker: {e}")))?
        }
        Err(error) => return Err(failure(format!("connect worker: {error}"))),
    };
    nonblocking(&stream)?;
    send(&mut stream, request, deadline)?;
    match receive(&mut stream, deadline)? {
        Reply::Success(result) => Ok(result),
        Reply::Failure(message) => Err(failure(message)),
    }
}

fn connect_socket(path: &Path) -> std::io::Result<UnixStream> {
    let metadata = std::fs::symlink_metadata(path)?;
    // SAFETY: geteuid has no arguments or memory access requirements.
    if !metadata.file_type().is_socket()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o777 != 0o600
    {
        return Err(std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            "worker socket must be owned, non-symlink and 0600",
        ));
    }
    UnixStream::connect(path)
}

fn launch(root: &Path, deadline: Instant) -> Result<(), XrayError> {
    let mut command = Command::new(std::env::current_exe().map_err(|e| failure(e.to_string()))?);
    command
        .arg("--lsp-worker")
        .arg(root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    std::os::unix::process::CommandExt::process_group(&mut command, 0);
    let mut child = command
        .spawn()
        .map_err(|e| failure(format!("spawn worker: {e}")))?;
    let result = (|| {
        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| failure("worker has no stdout"))?;
        nonblocking(&stdout)?;
        let mut byte = [0];
        if read_until(&mut stdout, &mut byte, deadline)? != 1 || byte != *b"R" {
            return Err(failure("worker exited before startup acknowledgment"));
        }
        Ok(())
    })();
    if result.is_err() {
        let _ = child.kill();
        let _ = child.wait();
    }
    result
}

struct SocketCleanup(PathBuf);
impl Drop for SocketCleanup {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

pub(super) fn run(root: &Path) -> Result<(), XrayError> {
    let root = root.canonicalize().map_err(|e| failure(e.to_string()))?;
    if !root.is_dir() {
        return Err(failure("worker root is not a directory"));
    }
    let paths = paths(&root)?;
    let _owner = lock(&paths.directory.join("worker.lock"), Instant::now(), false)?;
    // Only the worker-lock owner can remove a stale socket. Queued clients never unlink it.
    match std::fs::remove_file(&paths.socket) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(failure(e.to_string())),
    }
    let listener =
        UnixListener::bind(&paths.socket).map_err(|e| failure(format!("bind worker: {e}")))?;
    let _cleanup = SocketCleanup(paths.socket.clone());
    std::fs::set_permissions(&paths.socket, std::fs::Permissions::from_mode(0o600))
        .map_err(|e| failure(e.to_string()))?;
    nonblocking(&listener)?;
    std::io::stdout()
        .write_all(b"R")
        .and_then(|()| std::io::stdout().flush())
        .map_err(|e| failure(e.to_string()))?;
    let mut workspace = None;
    loop {
        if wait_for_connection(&listener, &mut workspace, Instant::now() + IDLE_TIMEOUT).is_err() {
            break;
        }
        let (mut stream, _) = listener.accept().map_err(|e| failure(e.to_string()))?;
        nonblocking(&stream)?;
        let deadline = Instant::now() + QUERY_TIMEOUT;
        let result = receive::<Request>(&mut stream, deadline).and_then(|request| {
            if workspace.is_none() {
                workspace = Some(Workspace::start(&root, &paths.directory)?);
            }
            workspace
                .as_mut()
                .ok_or_else(|| failure("workspace initialization failed"))?
                .query(&root, &request)
        });
        let reply = match result {
            Ok(result) => success_reply(result, MAX_MESSAGE),
            Err(error) => {
                // A timeout/protocol failure may desynchronize the server; never reuse it.
                workspace = None;
                Reply::Failure(error.to_string())
            }
        };
        let _ = send(&mut stream, &reply, deadline);
    }
    Ok(())
}

// Supplemental heuristics must never crowd exact LSP locations out of the transport.
fn success_reply(result: NavigationResult, limit: usize) -> Reply {
    let mut reply = Reply::Success(result);
    let fits = |reply: &Reply| serde_json::to_vec(reply).is_ok_and(|bytes| bytes.len() <= limit);
    if fits(&reply) {
        return reply;
    }
    if let Reply::Success(result) = &mut reply {
        if let Some(supplement) = result.conventions.as_mut() {
            supplement.omitted += supplement.results.len();
            supplement.count = 0;
            supplement.results.clear();
            supplement.parser_recovery_files.clear();
            supplement.target_class = supplement.target_class.chars().take(256).collect();
            supplement.status = "partial".into();
            supplement.warnings = vec![
                "convention payload omitted at worker limit; exact LSP results preserved".into(),
            ];
        }
    }
    if fits(&reply) {
        reply
    } else {
        Reply::Failure("exact navigation result exceeds worker message limit".into())
    }
}

fn wait_for_connection(
    listener: &UnixListener,
    workspace: &mut Option<Workspace>,
    deadline: Instant,
) -> Result<(), XrayError> {
    loop {
        if Instant::now() >= deadline {
            return Err(failure("worker idle deadline exceeded"));
        }
        let buffered = workspace
            .as_ref()
            .is_some_and(|w| w.client.has_buffered_output());
        if !buffered {
            let mut descriptors = [
                libc::pollfd {
                    fd: listener.as_raw_fd(),
                    events: libc::POLLIN,
                    revents: 0,
                },
                libc::pollfd {
                    fd: workspace.as_ref().map_or(-1, |w| w.client.output_fd()),
                    events: libc::POLLIN,
                    revents: 0,
                },
            ];
            poll_descriptors(&mut descriptors, deadline)?;
            if descriptors[0].revents != 0 {
                return Ok(());
            }
        }
        // Keep draining progress/configuration while idle so indexing cannot block on stdout.
        if let Some(state) = workspace.as_mut() {
            if state
                .client
                .receive_notification(deadline.min(Instant::now() + TIMEOUT))
                .is_err()
            {
                *workspace = None;
            }
        }
    }
}

fn send(
    stream: &mut UnixStream,
    value: &impl Serialize,
    deadline: Instant,
) -> Result<(), XrayError> {
    let bytes = serde_json::to_vec(value).map_err(|e| failure(e.to_string()))?;
    if bytes.len() > MAX_MESSAGE {
        return Err(failure("worker message exceeds 16 MiB"));
    }
    let size = u32::try_from(bytes.len()).map_err(|e| failure(e.to_string()))?;
    write_until(stream, &size.to_be_bytes(), deadline)?;
    write_until(stream, &bytes, deadline)
}

fn receive<T: DeserializeOwned>(
    stream: &mut UnixStream,
    deadline: Instant,
) -> Result<T, XrayError> {
    let mut header = [0; 4];
    read_exact(stream, &mut header, deadline)?;
    let length = usize::try_from(u32::from_be_bytes(header)).map_err(|e| failure(e.to_string()))?;
    if length > MAX_MESSAGE {
        return Err(failure("worker message exceeds 16 MiB"));
    }
    let mut bytes = vec![0; length];
    read_exact(stream, &mut bytes, deadline)?;
    serde_json::from_slice(&bytes).map_err(|e| failure(format!("worker JSON: {e}")))
}

fn read_exact(
    stream: &mut UnixStream,
    mut bytes: &mut [u8],
    deadline: Instant,
) -> Result<(), XrayError> {
    while !bytes.is_empty() {
        let count = read_until(stream, bytes, deadline)?;
        if count == 0 {
            return Err(failure("worker closed connection before responding"));
        }
        bytes = &mut bytes[count..];
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oversized_conventions_cannot_displace_exact_locations() {
        use crate::navigation::{ConventionCandidate, ConventionResult, NavigationLocation};
        let location = || NavigationLocation {
            file: "/tmp/source.php".into(),
            line: 1,
            column: 1,
            end_line: 1,
            end_column: 2,
        };
        let result = NavigationResult {
            locations: vec![location()],
            provenance: "lsp:intelephense".into(),
            readiness: "ready".into(),
            warnings: Vec::new(),
            conventions: Some(Box::new(ConventionResult {
                kind: "eloquent-scope".into(),
                target_class: "App\\Models\\X".into(),
                status: "complete".into(),
                coverage: "candidates only".into(),
                count: 1,
                omitted: 0,
                scanned: 1,
                parser_recovery_files: Vec::new(),
                warnings: Vec::new(),
                results: vec![ConventionCandidate {
                    kind: "scope-name-match".into(),
                    subject: "x".repeat(5000),
                    provenance: "syntax:laravel-convention-candidate".into(),
                    location: location(),
                }],
            })),
        };
        let reply = success_reply(result, 2048);
        assert!(serde_json::to_vec(&reply).unwrap().len() <= 2048);
        let Reply::Success(result) = reply else {
            panic!("lost exact results");
        };
        assert_eq!(result.locations.len(), 1);
        let supplement = result.conventions.unwrap();
        assert_eq!(supplement.status, "partial");
        assert_eq!(supplement.omitted, 1);
        assert_eq!(supplement.count, 0);
        assert!(!supplement.warnings.is_empty());
    }

    #[test]
    fn licence_activation_and_replacement_change_session_identity_without_reading_key() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("licence.txt");
        let fingerprint = |path| {
            let mut hash = std::collections::hash_map::DefaultHasher::new();
            hash_licence(path, &mut hash).unwrap();
            hash.finish()
        };
        let free = fingerprint(None);
        std::fs::write(&file, "test-only").unwrap();
        let active = fingerprint(Some(file.as_path()));
        assert_ne!(free, active);
        assert_eq!(active, fingerprint(Some(file.as_path())));
        let replacement = dir.path().join("replacement");
        std::fs::write(&replacement, "test-only").unwrap();
        std::fs::rename(replacement, &file).unwrap();
        assert_ne!(active, fingerprint(Some(file.as_path())));
        assert_eq!(free, fingerprint(None));
    }

    #[test]
    fn private_directory_rejects_symlinks_and_public_permissions() {
        let dir = tempfile::tempdir().unwrap();
        let private = dir.path().join("private");
        private_directory(&private).unwrap();
        assert_eq!(private.metadata().unwrap().mode() & 0o777, 0o700);
        let link = dir.path().join("link");
        std::os::unix::fs::symlink(&private, &link).unwrap();
        assert!(private_directory(&link).is_err());
        std::fs::set_permissions(&private, std::fs::Permissions::from_mode(0o755)).unwrap();
        assert!(private_directory(&private).is_err());
    }

    #[test]
    fn lock_rejects_symlinks_and_recovers_after_owner_drop() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("owner.lock");
        let owner = lock(&path, Instant::now(), false).unwrap();
        assert!(lock(&path, Instant::now(), false).is_err());
        drop(owner);
        assert!(lock(&path, Instant::now(), false).is_ok());
        let link = dir.path().join("link.lock");
        std::os::unix::fs::symlink(&path, &link).unwrap();
        assert!(lock(&link, Instant::now(), false).is_err());
    }

    #[test]
    fn local_protocol_rejects_oversized_message_and_timeout() {
        let (mut writer, mut reader) = UnixStream::pair().unwrap();
        nonblocking(&writer).unwrap();
        nonblocking(&reader).unwrap();
        let deadline = Instant::now() + Duration::from_millis(40);
        assert!(receive::<serde_json::Value>(&mut reader, deadline)
            .unwrap_err()
            .to_string()
            .contains("deadline"));
        write_until(
            &mut writer,
            &u32::MAX.to_be_bytes(),
            Instant::now() + Duration::from_secs(1),
        )
        .unwrap();
        assert!(
            receive::<serde_json::Value>(&mut reader, Instant::now() + Duration::from_secs(1))
                .unwrap_err()
                .to_string()
                .contains("exceeds")
        );
    }
}
