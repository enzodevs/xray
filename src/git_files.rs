//! Shared Git-visible enumeration; never silently scan a broken repository unfiltered.
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use crate::error::XrayError;

pub(crate) fn visible(root: &Path) -> Result<Option<BTreeSet<PathBuf>>, XrayError> {
    let mut command = Command::new("git");
    command
        .args([
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "-z",
            "--",
            ".",
        ])
        .current_dir(root);
    let output = bounded_output(&mut command);
    match output {
        Ok(output) if output.status.success() => {
            let mut paths = BTreeSet::new();
            for name in output
                .stdout
                .split(|byte| *byte == 0)
                .filter(|name| !name.is_empty())
            {
                #[cfg(unix)]
                let name = {
                    use std::os::unix::ffi::OsStrExt;
                    std::ffi::OsStr::from_bytes(name)
                };
                #[cfg(not(unix))]
                let name = std::str::from_utf8(name)
                    .map(std::ffi::OsStr::new)
                    .map_err(|e| XrayError::ParseFailed(e.to_string()))?;
                paths.insert(root.join(name));
            }
            Ok(Some(paths))
        }
        result => {
            if root.ancestors().any(|path| path.join(".git").exists()) {
                return Err(XrayError::ParseFailed(format!(
                    "cannot enumerate Git files in {}; refusing an unfiltered scan: {}",
                    root.display(),
                    result.map_or_else(
                        |e| e.to_string(),
                        |out| String::from_utf8_lossy(&out.stderr).into_owned()
                    )
                )));
            }
            Ok(None)
        }
    }
}

#[cfg(not(unix))]
fn bounded_output(command: &mut Command) -> Result<Output, XrayError> {
    command
        .output()
        .map_err(|e| XrayError::ParseFailed(e.to_string()))
}

#[cfg(unix)]
fn bounded_output(command: &mut Command) -> Result<Output, XrayError> {
    use crate::lsp::{failure, nonblocking, read_until};
    use std::process::Stdio;
    use std::time::{Duration, Instant};
    let deadline = Instant::now() + Duration::from_secs(15);
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| failure(e.to_string()))?;
    let result = (|| {
        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| failure("Git has no stdout"))?;
        nonblocking(&stdout)?;
        let mut bytes = Vec::new();
        let mut buffer = [0; 8192];
        loop {
            let count = read_until(&mut stdout, &mut buffer, deadline)?;
            if count == 0 {
                break;
            }
            if bytes.len() + count > 16 * 1024 * 1024 {
                return Err(failure("Git listing exceeds 16 MiB"));
            }
            bytes.extend_from_slice(&buffer[..count]);
        }
        loop {
            if let Some(status) = child.try_wait().map_err(|e| failure(e.to_string()))? {
                return Ok(Output {
                    status,
                    stdout: bytes,
                    stderr: Vec::new(),
                });
            }
            if Instant::now() >= deadline {
                return Err(failure("Git listing deadline exceeded"));
            }
            std::thread::sleep(Duration::from_millis(1));
        }
    })();
    if result.is_err() {
        let _ = child.kill();
        let _ = child.wait();
    }
    result
}
