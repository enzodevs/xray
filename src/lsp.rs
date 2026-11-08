//! Bounded, synchronous JSON-RPC transport. No application or shell is executed.
use std::collections::HashMap;
use std::io::{Read, Write};
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::{json, Value};

use crate::error::XrayError;
use crate::lang::LanguageKind;

pub(crate) const TIMEOUT: Duration = Duration::from_secs(90);
pub(crate) const MAX_MESSAGE: usize = 16 * 1024 * 1024;

pub(crate) fn failure(message: impl Into<String>) -> XrayError {
    XrayError::Lsp(message.into())
}

/// An exact LSP location, with zero-based UTF-16 positions.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Location {
    pub file: PathBuf,
    pub line: usize,
    pub character: usize,
    pub end_line: usize,
    pub end_character: usize,
}

/// Language-server process with bounded reads, writes and shutdown.
pub struct LspClient {
    child: Child,
    stopped: bool,
    stdin: ChildStdin,
    stdout: ChildStdout,
    buffered: Vec<u8>,
    next_id: u64,
    opened_files: HashMap<PathBuf, (i64, String)>,
    capabilities: Value,
    configuration: Value,
    progress: std::collections::HashSet<String>,
    index_tokens: std::collections::HashSet<String>,
    pub(crate) index_completed: bool,
}

impl LspClient {
    /// Spawn a supported language server and negotiate UTF-16 positions.
    pub fn start(root: &Path, language: LanguageKind) -> Result<Self, XrayError> {
        let (program, args) = server_command(language)?;
        let options = initialization_options(language)?;
        Self::start_command(root, &program, args, &options, None)
    }

    pub(crate) fn start_php(root: &Path, storage: &Path) -> Result<Self, XrayError> {
        let program = php_executable()?;
        let program = program
            .to_str()
            .ok_or_else(|| failure("Intelephense executable path is not UTF-8"))?;
        let options = json!({"storagePath":storage,"globalStoragePath":storage});
        Self::start_command(root, program, &["--stdio"], &options, Some(storage))
    }

    fn start_command(
        root: &Path,
        program: &str,
        args: &[&str],
        initialization_options: &Value,
        storage: Option<&Path>,
    ) -> Result<Self, XrayError> {
        let mut command = Command::new(program);
        command
            .args(args)
            .current_dir(root)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null());
        // A dedicated process group lets cleanup also close pipes inherited by server children.
        std::os::unix::process::CommandExt::process_group(&mut command, 0);
        let mut child = command
            .spawn()
            .map_err(|e| failure(format!("{program} spawn failed: {e}")))?;
        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| failure("server has no stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| failure("server has no stdout"))?;
        let mut client = Self {
            child,
            stopped: false,
            stdin,
            stdout,
            buffered: Vec::new(),
            next_id: 1,
            opened_files: HashMap::new(),
            capabilities: Value::Null,
            configuration: storage.map_or(Value::Null, |_| php_configuration()),
            progress: std::collections::HashSet::new(),
            index_tokens: std::collections::HashSet::new(),
            index_completed: false,
        };
        nonblocking(&client.stdin)?;
        nonblocking(&client.stdout)?;
        let uri = path_to_uri(root)?;
        let result = client.request(
            "initialize",
            &json!({
                "processId": std::process::id(), "rootUri": uri,
                "initializationOptions": initialization_options,
                "workspaceFolders": [{"uri":uri,"name":"workspace"}],
                "capabilities": {
                    "general":{"positionEncodings":["utf-16"]},
                    "window":{"workDoneProgress":true},
                    "workspace":{"configuration":true,"workspaceFolders":true},
                    "textDocument":{"definition":{"linkSupport":true}}
                }
            }),
        )?;
        client.capabilities = result
            .get("capabilities")
            .cloned()
            .ok_or_else(|| failure("initialize omitted capabilities"))?;
        if let Some(encoding) = client
            .capabilities
            .get("positionEncoding")
            .and_then(Value::as_str)
        {
            if encoding != "utf-16" {
                return Err(failure(format!(
                    "unsupported position encoding: {encoding}"
                )));
            }
        }
        client.notify("initialized", &json!({}))?;
        if !client.configuration.is_null() {
            client.notify(
                "workspace/didChangeConfiguration",
                &json!({"settings":client.configuration}),
            )?;
        }
        Ok(client)
    }

    pub(crate) fn restart_php(&mut self, root: &Path, storage: &Path) -> Result<(), XrayError> {
        self.stop();
        *self = Self::start_php(root, storage)?;
        Ok(())
    }

    fn stop(&mut self) {
        if self.stopped {
            return;
        }
        if let Ok(pid) = i32::try_from(self.child.id()) {
            // SAFETY: child created its own process group; negative PID targets only that group.
            unsafe {
                libc::kill(-pid, libc::SIGKILL);
            }
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
        self.stopped = true;
    }

    /// Legacy trace adapter. Input is zero-based UTF-8 byte offsets.
    /// Ambiguous definitions deliberately remain unresolved.
    pub fn definition(&mut self, file: &Path, line: usize, col: usize) -> Option<(PathBuf, usize)> {
        if self.stopped {
            return None;
        }
        match self.trace_definition(file, line, col) {
            Ok(location) => location,
            Err(error) => {
                crate::presentation::warn(format!(
                    "LSP trace fallback at {}:{}:{}: {error}",
                    file.display(),
                    line + 1,
                    col + 1
                ));
                // A broken transport must not cost another full timeout for every call in the trace.
                self.stop();
                None
            }
        }
    }

    fn trace_definition(
        &mut self,
        file: &Path,
        line: usize,
        col: usize,
    ) -> Result<Option<(PathBuf, usize)>, XrayError> {
        let text = self.ensure_open(file)?;
        let character = byte_to_utf16(&text, line, col)?;
        let mut locations = self.locations(file, line, character, false)?;
        if locations.len() > 1 {
            crate::presentation::warn(format!(
                "ambiguous LSP trace definition at {}:{}:{} ({} candidates)",
                file.display(),
                line + 1,
                col + 1,
                locations.len()
            ));
            return Ok(None);
        }
        Ok(locations
            .pop()
            .map(|location| (location.file, location.line)))
    }

    pub(crate) fn locations(
        &mut self,
        file: &Path,
        line: usize,
        character: usize,
        references: bool,
    ) -> Result<Vec<Location>, XrayError> {
        let capability = if references {
            "referencesProvider"
        } else {
            "definitionProvider"
        };
        if !self
            .capabilities
            .get(capability)
            .is_some_and(|v| v == true || v.is_object())
        {
            return Err(failure(format!("server does not support {capability}")));
        }
        let method = if references {
            "textDocument/references"
        } else {
            "textDocument/definition"
        };
        let mut params = json!({
            "textDocument":{"uri":path_to_uri(file)?},
            "position":{"line":line,"character":character}
        });
        if references {
            params["context"] = json!({"includeDeclaration":false});
        }
        let deadline = Instant::now() + TIMEOUT;
        let mut result = self.request_until(method, &params, deadline)?;
        // Wait only for actual server progress, never a guessed sleep. Requery after indexing.
        while self.indexing() {
            while self.indexing() {
                self.receive_notification(deadline)?;
            }
            result = self.request_until(method, &params, deadline)?;
        }
        parse_locations(&result)
    }

    /// Open imports before definition requests, without guessed indexing sleeps.
    pub fn warm_up(&mut self, file: &Path, imports: &[PathBuf]) {
        for path in std::iter::once(file).chain(imports.iter().map(PathBuf::as_path)) {
            if let Err(error) = self.ensure_open(path) {
                crate::presentation::warn(format!("LSP trace warm-up failed: {error}"));
                self.stop();
                break;
            }
        }
    }

    /// Refresh an open document from disk, retaining monotonically increasing versions.
    pub(crate) fn ensure_open(&mut self, file: &Path) -> Result<String, XrayError> {
        let canonical = file
            .canonicalize()
            .map_err(|e| failure(format!("{}: {e}", file.display())))?;
        let text = read_source(&canonical)?;
        let uri = path_to_uri(&canonical)?;
        let version = if let Some((version, old)) = self.opened_files.get(&canonical) {
            if old == &text {
                return Ok(text);
            }
            let version = version + 1;
            self.notify("textDocument/didChange", &json!({"textDocument":{"uri":uri,"version":version},"contentChanges":[{"text":text}]}))?;
            version
        } else {
            self.notify("textDocument/didOpen", &json!({"textDocument":{"uri":uri,"languageId":language_id(&canonical),"version":1,"text":text}}))?;
            1
        };
        self.opened_files.insert(canonical, (version, text.clone()));
        Ok(text)
    }

    pub(crate) fn close(&mut self, file: &Path) -> Result<(), XrayError> {
        self.notify(
            "textDocument/didClose",
            &json!({"textDocument":{"uri":path_to_uri(file)?}}),
        )?;
        self.opened_files.remove(file);
        Ok(())
    }

    pub(crate) fn has_buffered_output(&self) -> bool {
        !self.buffered.is_empty()
    }

    pub(crate) fn output_fd(&self) -> std::os::fd::RawFd {
        self.stdout.as_raw_fd()
    }

    pub(crate) fn receive_notification(&mut self, deadline: Instant) -> Result<(), XrayError> {
        let message = self.read_message(deadline)?;
        self.handle_server_message(&message, deadline)
    }

    /// Wait for actual Intelephense index progress, including cached 0/0 indexing.
    pub(crate) fn wait_for_initial_index(&mut self) -> Result<(), XrayError> {
        let deadline = Instant::now() + TIMEOUT;
        while !self.index_completed || self.indexing() {
            self.receive_notification(deadline)?;
        }
        Ok(())
    }

    /// A completed progress token is evidence of activity, not project completeness.
    pub(crate) fn indexing(&self) -> bool {
        !self.progress.is_empty()
    }

    pub fn shutdown(mut self) {
        let deadline = Instant::now() + Duration::from_secs(2);
        let _ = self.request_until("shutdown", &Value::Null, deadline);
        let _ = self.send(&json!({"jsonrpc":"2.0","method":"exit"}), deadline);
        // Drop kills/reaps even servers that acknowledge shutdown but never exit.
    }

    pub(crate) fn request(&mut self, method: &str, params: &Value) -> Result<Value, XrayError> {
        self.request_until(method, params, Instant::now() + TIMEOUT)
    }

    fn request_until(
        &mut self,
        method: &str,
        params: &Value,
        deadline: Instant,
    ) -> Result<Value, XrayError> {
        let id = self.next_id;
        self.next_id += 1;
        self.send(
            &json!({"jsonrpc":"2.0","id":id,"method":method,"params":params}),
            deadline,
        )?;
        loop {
            let message = self.read_message(deadline)?;
            if message.get("method").is_some() {
                self.handle_server_message(&message, deadline)?;
            } else if message.get("id").and_then(Value::as_u64) == Some(id) {
                if let Some(error) = message.get("error") {
                    return Err(failure(format!("{method}: server error: {error}")));
                }
                return message
                    .get("result")
                    .cloned()
                    .ok_or_else(|| failure("response omitted result"));
            }
        }
    }

    fn handle_server_message(
        &mut self,
        message: &Value,
        deadline: Instant,
    ) -> Result<(), XrayError> {
        let method = message["method"].as_str().unwrap_or_default();
        if method == "$/progress" {
            let token = message["params"]["token"].to_string();
            match message["params"]["value"]["kind"].as_str() {
                Some("begin") => {
                    if message["params"]["value"]["title"]
                        .as_str()
                        .is_some_and(|title| title.to_ascii_lowercase().contains("index"))
                    {
                        self.index_tokens.insert(token.clone());
                    }
                    self.progress.insert(token);
                }
                Some("end") => {
                    self.progress.remove(&token);
                    if self.index_tokens.remove(&token) {
                        self.index_completed = true;
                    }
                }
                _ => {}
            }
        }
        if let Some(id) = message.get("id") {
            let result = match method {
                "workspace/configuration" => {
                    let items = message["params"]["items"]
                        .as_array()
                        .ok_or_else(|| failure("invalid configuration request"))?;
                    Value::Array(
                        items
                            .iter()
                            .map(|item| {
                                configuration_section(
                                    &self.configuration,
                                    item.get("section").and_then(Value::as_str),
                                )
                            })
                            .collect(),
                    )
                }
                "client/registerCapability"
                | "client/unregisterCapability"
                | "window/workDoneProgress/create" => Value::Null,
                "workspace/applyEdit" => {
                    json!({"applied":false,"failureReason":"xray is read-only"})
                }
                _ => {
                    return self.send(&json!({"jsonrpc":"2.0","id":id,"error":{"code":-32601,"message":"unsupported client method"}}),deadline);
                }
            };
            self.send(&json!({"jsonrpc":"2.0","id":id,"result":result}), deadline)?;
        }
        Ok(())
    }

    pub(crate) fn notify(&mut self, method: &str, params: &Value) -> Result<(), XrayError> {
        self.send(
            &json!({"jsonrpc":"2.0","method":method,"params":params}),
            Instant::now() + TIMEOUT,
        )
    }

    fn send(&mut self, message: &Value, deadline: Instant) -> Result<(), XrayError> {
        let body = serde_json::to_vec(message).map_err(|e| failure(e.to_string()))?;
        if body.len() > MAX_MESSAGE {
            return Err(failure("outgoing message exceeds 16 MiB"));
        }
        write_until(
            &mut self.stdin,
            format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes(),
            deadline,
        )?;
        write_until(&mut self.stdin, &body, deadline)
    }

    fn read_message(&mut self, deadline: Instant) -> Result<Value, XrayError> {
        let header_end = loop {
            if let Some(end) = self.buffered.windows(4).position(|w| w == b"\r\n\r\n") {
                break end + 4;
            }
            if self.buffered.len() > 8192 {
                return Err(failure("LSP header exceeds 8 KiB"));
            }
            self.read_more(deadline)?;
        };
        if header_end > 8192 {
            return Err(failure("LSP header exceeds 8 KiB"));
        }
        let header = std::str::from_utf8(&self.buffered[..header_end])
            .map_err(|e| failure(e.to_string()))?;
        let mut length = None;
        for line in header.split("\r\n") {
            if let Some((name, value)) = line.split_once(':') {
                if name.eq_ignore_ascii_case("Content-Length") {
                    if length.is_some() {
                        return Err(failure("duplicate Content-Length"));
                    }
                    length = Some(
                        value
                            .trim()
                            .parse::<usize>()
                            .map_err(|e| failure(format!("invalid Content-Length: {e}")))?,
                    );
                }
            }
        }
        let length = length.ok_or_else(|| failure("missing Content-Length"))?;
        if length > MAX_MESSAGE {
            return Err(failure("LSP message exceeds 16 MiB"));
        }
        while self.buffered.len() < header_end + length {
            self.read_more(deadline)?;
        }
        let result = serde_json::from_slice(&self.buffered[header_end..header_end + length])
            .map_err(|e| failure(format!("invalid JSON: {e}")))?;
        self.buffered.drain(..header_end + length);
        Ok(result)
    }

    fn read_more(&mut self, deadline: Instant) -> Result<(), XrayError> {
        let mut bytes = [0; 8192];
        let count = read_until(&mut self.stdout, &mut bytes, deadline)?;
        if count == 0 {
            return Err(failure("server closed stdout before responding"));
        }
        self.buffered.extend_from_slice(&bytes[..count]);
        Ok(())
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        self.stop();
    }
}

pub(crate) fn nonblocking(fd: &impl AsRawFd) -> Result<(), XrayError> {
    // SAFETY: the borrowed descriptor remains valid throughout both calls.
    let flags = unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_GETFL) };
    if flags < 0
        || unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFL, flags | libc::O_NONBLOCK) } < 0
    {
        return Err(failure(std::io::Error::last_os_error().to_string()));
    }
    Ok(())
}

pub(crate) fn wait_fd(fd: &impl AsRawFd, events: i16, deadline: Instant) -> Result<(), XrayError> {
    poll_descriptors(
        &mut [libc::pollfd {
            fd: fd.as_raw_fd(),
            events,
            revents: 0,
        }],
        deadline,
    )
}

pub(crate) fn poll_descriptors(
    descriptors: &mut [libc::pollfd],
    deadline: Instant,
) -> Result<(), XrayError> {
    loop {
        let remaining = deadline
            .checked_duration_since(Instant::now())
            .ok_or_else(|| failure("deadline exceeded"))?;
        let millis = i32::try_from(remaining.as_millis().max(1)).unwrap_or(i32::MAX);
        let count =
            libc::nfds_t::try_from(descriptors.len()).map_err(|e| failure(e.to_string()))?;
        // SAFETY: the initialized slice is live for the call; all descriptors are borrowed.
        let result = unsafe { libc::poll(descriptors.as_mut_ptr(), count, millis) };
        if result > 0 {
            return Ok(());
        }
        if result == 0 {
            return Err(failure("deadline exceeded"));
        }
        let error = std::io::Error::last_os_error();
        if error.kind() != std::io::ErrorKind::Interrupted {
            return Err(failure(error.to_string()));
        }
    }
}

pub(crate) fn read_until(
    reader: &mut (impl Read + AsRawFd),
    bytes: &mut [u8],
    deadline: Instant,
) -> Result<usize, XrayError> {
    loop {
        wait_fd(reader, libc::POLLIN, deadline)?;
        match reader.read(bytes) {
            Ok(size) => return Ok(size),
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::Interrupted
                ) => {}
            Err(e) => return Err(failure(format!("read: {e}"))),
        }
    }
}

pub(crate) fn write_until(
    writer: &mut (impl Write + AsRawFd),
    mut bytes: &[u8],
    deadline: Instant,
) -> Result<(), XrayError> {
    while !bytes.is_empty() {
        wait_fd(writer, libc::POLLOUT, deadline)?;
        match writer.write(bytes) {
            Ok(0) => return Err(failure("server closed input")),
            Ok(size) => bytes = &bytes[size..],
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::Interrupted
                ) => {}
            Err(e) => return Err(failure(format!("write: {e}"))),
        }
    }
    Ok(())
}

/// Retain documented server defaults, but exclude hidden caches like the freshness scanner.
/// Vendor remains indexed: Laravel navigation needs dependency declarations and `PHPDoc` types.
fn php_configuration() -> Value {
    json!({"intelephense":{"files":{"exclude":[
        "**/.*/**", "**/.git/**", "**/.svn/**", "**/.hg/**", "**/CVS/**",
        "**/.DS_Store/**", "**/node_modules/**", "**/bower_components/**",
        "**/vendor/**/{Tests,tests}/**", "**/.history/**", "**/vendor/**/vendor/**"
    ]}}})
}

fn configuration_section(configuration: &Value, section: Option<&str>) -> Value {
    section.map_or_else(
        || configuration.clone(),
        |section| {
            section
                .split('.')
                .try_fold(configuration, |value, key| value.get(key))
                .cloned()
                .unwrap_or(Value::Null)
        },
    )
}

pub(crate) fn php_executable() -> Result<PathBuf, XrayError> {
    let program = std::env::var_os("XRAY_INTELEPHENSE").unwrap_or_else(|| "intelephense".into());
    let path = Path::new(&program);
    if path.components().count() > 1 || path.is_absolute() {
        return path
            .canonicalize()
            .map_err(|e| failure(format!("Intelephense executable: {e}")));
    }
    find_executable(path)
        .ok_or_else(|| failure("intelephense not found on PATH (or set XRAY_INTELEPHENSE)"))
}

/// Resolve an executable on PATH to its canonical (symlink-free) location.
fn find_executable(name: &Path) -> Option<PathBuf> {
    use std::os::unix::fs::PermissionsExt;
    std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default())
        .map(|directory| directory.join(name))
        .find(|candidate| {
            candidate
                .metadata()
                .is_ok_and(|m| m.is_file() && m.permissions().mode() & 0o111 != 0)
        })?
        .canonicalize()
        .ok()
}

/// Vue support follows the official hybrid setup: tsserver loads `@vue/typescript-plugin`
/// from the `@vue/language-server` package, so Vue SFC script positions resolve through TypeScript.
fn initialization_options(language: LanguageKind) -> Result<Value, XrayError> {
    match language {
        LanguageKind::Vue => {
            let location = vue_plugin_location()?.ok_or_else(|| {
                failure(
                    "@vue/language-server not found: install it so vue-language-server is on PATH \
                     (or set XRAY_VUE_LANGUAGE_SERVER to the package directory)",
                )
            })?;
            Ok(vue_initialization_options(&location))
        }
        LanguageKind::Ts => Ok(vue_plugin_location()?
            .map_or_else(typescript_initialization_options, |location| {
                vue_initialization_options(&location)
            })),
        _ => Ok(json!({})),
    }
}

fn typescript_initialization_options() -> Value {
    // The server's default "auto" routes early definitions to a syntax-only server
    // while the project loads. Batch navigation needs semantic answers, not aliases
    // pointing back to local imports/bindings. Requests remain deadline-bounded.
    json!({"tsserver":{"useSyntaxServer":"never"}})
}

fn vue_initialization_options(location: &Path) -> Value {
    let mut options = typescript_initialization_options();
    options["plugins"] = json!([{
        "name":"@vue/typescript-plugin",
        "location":location,
        "languages":["vue"],
        "configNamespace":"typescript"
    }]);
    options
}

fn vue_plugin_location() -> Result<Option<PathBuf>, XrayError> {
    if let Some(directory) = std::env::var_os("XRAY_VUE_LANGUAGE_SERVER") {
        let directory = PathBuf::from(directory);
        if !is_vue_language_server_package(&directory) {
            return Err(failure(format!(
                "XRAY_VUE_LANGUAGE_SERVER is not an @vue/language-server package: {}",
                directory.display()
            )));
        }
        return directory
            .canonicalize()
            .map(Some)
            .map_err(|error| failure(error.to_string()));
    }
    // The binary lives at `<package>/bin/vue-language-server.js`; locate the owning package.
    Ok(
        find_executable(Path::new("vue-language-server")).and_then(|executable| {
            executable
                .ancestors()
                .find(|directory| is_vue_language_server_package(directory))
                .map(Path::to_path_buf)
        }),
    )
}

fn is_vue_language_server_package(directory: &Path) -> bool {
    std::fs::read_to_string(directory.join("package.json"))
        .ok()
        .and_then(|text| serde_json::from_str::<Value>(&text).ok())
        .is_some_and(|package| package["name"] == "@vue/language-server")
}

fn server_command(language: LanguageKind) -> Result<(String, &'static [&'static str]), XrayError> {
    match language {
        LanguageKind::Php => Ok((
            std::env::var("XRAY_INTELEPHENSE").unwrap_or_else(|_| "intelephense".into()),
            &["--stdio"],
        )),
        LanguageKind::Ts | LanguageKind::Vue => {
            Ok(("typescript-language-server".into(), &["--stdio"]))
        }
        LanguageKind::Go => Ok(("gopls".into(), &["serve"])),
        LanguageKind::Svelte => Ok(("svelte-language-server".into(), &["--stdio"])),
        _ => Err(failure(format!(
            "no LSP backend for {}",
            language.display_name()
        ))),
    }
}

fn language_id(path: &Path) -> &'static str {
    match path
        .extension()
        .and_then(|s| s.to_str())
        .map(str::to_ascii_lowercase)
        .as_deref()
    {
        Some("ts" | "mts" | "cts") => "typescript",
        Some("tsx") => "typescriptreact",
        Some("jsx") => "javascriptreact",
        Some("php") => "php",
        Some("go") => "go",
        Some("svelte") => "svelte",
        Some("vue") => "vue",
        _ => "javascript",
    }
}

pub(crate) fn path_to_uri(path: &Path) -> Result<String, XrayError> {
    url::Url::from_file_path(path)
        .map(String::from)
        .map_err(|()| failure(format!("not an absolute file path: {}", path.display())))
}

pub(crate) fn parse_locations(value: &Value) -> Result<Vec<Location>, XrayError> {
    if value.is_null() {
        return Ok(Vec::new());
    }
    let values = value
        .as_array()
        .map_or_else(|| std::slice::from_ref(value), Vec::as_slice);
    let mut locations = values
        .iter()
        .map(|location| {
            let linked = location.get("targetUri").is_some();
            let uri = location[if linked { "targetUri" } else { "uri" }]
                .as_str()
                .ok_or_else(|| failure("location omitted URI"))?;
            let file = url::Url::parse(uri)
                .map_err(|e| failure(e.to_string()))?
                .to_file_path()
                .map_err(|()| failure(format!("not a local file URI: {uri}")))?;
            let range = &location[if linked {
                "targetSelectionRange"
            } else {
                "range"
            }];
            let number = |position: &str, field: &str| -> Result<usize, XrayError> {
                range[position][field]
                    .as_u64()
                    .and_then(|v| usize::try_from(v).ok())
                    .ok_or_else(|| failure("invalid location position"))
            };
            let location = Location {
                file,
                line: number("start", "line")?,
                character: number("start", "character")?,
                end_line: number("end", "line")?,
                end_character: number("end", "character")?,
            };
            if (location.end_line, location.end_character) < (location.line, location.character) {
                return Err(failure("reversed location range"));
            }
            Ok(location)
        })
        .collect::<Result<Vec<_>, XrayError>>()?;
    locations.sort_unstable();
    locations.dedup();
    Ok(locations)
}

pub(crate) fn read_source(path: &Path) -> Result<String, XrayError> {
    use std::os::unix::fs::OpenOptionsExt;
    if path
        .file_name()
        .and_then(|s| s.to_str())
        .is_some_and(|s| s == ".env" || s.starts_with(".env."))
    {
        return Err(failure("refusing to read environment secret files"));
    }
    let file = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NONBLOCK)
        .open(path)
        .map_err(|e| failure(format!("{}: {e}", path.display())))?;
    if !file
        .metadata()
        .map_err(|e| failure(e.to_string()))?
        .is_file()
    {
        return Err(failure("source is not a regular file"));
    }
    let mut text = String::new();
    file.take((MAX_MESSAGE / 2 + 1) as u64)
        .read_to_string(&mut text)
        .map_err(|e| failure(format!("{}: {e}", path.display())))?;
    if text.len() > MAX_MESSAGE / 2 {
        return Err(failure("document exceeds 8 MiB"));
    }
    Ok(text)
}

fn source_line(text: &str, line: usize) -> Result<&str, XrayError> {
    text.split('\n')
        .nth(line)
        .map(|s| s.strip_suffix('\r').unwrap_or(s))
        .ok_or_else(|| failure("line is outside document"))
}

pub(crate) fn byte_to_utf16(text: &str, line: usize, column: usize) -> Result<usize, XrayError> {
    let line = source_line(text, line)?;
    let prefix = line
        .get(..column)
        .ok_or_else(|| failure("column is outside line or inside a UTF-8 codepoint"))?;
    Ok(prefix.encode_utf16().count())
}

pub(crate) fn utf16_to_byte(text: &str, line: usize, column: usize) -> Result<usize, XrayError> {
    let line = source_line(text, line)?;
    let mut units = 0;
    for (byte, ch) in line.char_indices() {
        if units == column {
            return Ok(byte);
        }
        units += ch.len_utf16();
    }
    if units == column {
        return Ok(line.len());
    }
    Err(failure(
        "server position is outside line or inside a UTF-16 surrogate pair",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mock(script: &str) -> (tempfile::TempDir, LspClient) {
        let root = tempfile::tempdir().unwrap();
        let code = format!(
            r"
import json, sys, time

def read():
    size = 0
    while True:
        line = sys.stdin.buffer.readline()
        if not line: sys.exit(0)
        if line == b'\r\n': break
        if line.lower().startswith(b'content-length:'): size = int(line.split(b':')[1])
    return json.loads(sys.stdin.buffer.read(size))

def send(msg):
    body = json.dumps(msg).encode()
    sys.stdout.buffer.write(('Content-Length: %d\r\n\r\n' % len(body)).encode()+body)
    sys.stdout.buffer.flush()

init=read()
send({{'jsonrpc':'2.0','id':init['id'],'result':{{'capabilities':{{'definitionProvider':True,'referencesProvider':True}}}}}})
read()
{script}
"
        );
        let client = LspClient::start_command(
            root.path(),
            "python3",
            &["-I", "-u", "-c", &code],
            &json!({}),
            None,
        )
        .unwrap();
        (root, client)
    }

    #[test]
    fn initial_index_wait_requires_observed_begin_and_end() {
        let (_root, mut client) = mock("send({'method':'$/progress','params':{'token':'index','value':{'kind':'begin','title':'Indexing'}}}); send({'method':'$/progress','params':{'token':'index','value':{'kind':'end'}}}); read()");
        assert!(!client.index_completed);
        client.wait_for_initial_index().unwrap();
        assert!(client.index_completed);
        assert!(!client.indexing());
    }

    #[test]
    fn missing_initial_index_signal_is_not_ready() {
        let (_root, mut client) = mock("sys.exit(0)");
        assert!(client.wait_for_initial_index().is_err());
        assert!(!client.index_completed);
    }

    #[test]
    fn blocked_server_input_obeys_write_deadline() {
        let (_root, mut client) = mock("time.sleep(60)");
        let started = Instant::now();
        let error = client
            .send(
                &json!({"text":"x".repeat(1024*1024)}),
                Instant::now() + Duration::from_millis(40),
            )
            .unwrap_err();
        assert!(error.to_string().contains("deadline"));
        drop(client);
        assert!(started.elapsed() < Duration::from_secs(2));
    }

    #[test]
    fn request_timeout_is_bounded_and_drop_reaps_server() {
        let (_root, mut client) = mock("read(); time.sleep(60)");
        let started = Instant::now();
        let error = client
            .request_until(
                "test",
                &Value::Null,
                Instant::now() + Duration::from_millis(40),
            )
            .unwrap_err();
        assert!(error.to_string().contains("deadline"));
        drop(client);
        assert!(started.elapsed() < Duration::from_secs(2));
    }

    #[test]
    fn early_exit_is_error_not_empty_locations() {
        let (_root, mut client) = mock("sys.exit(0)");
        assert!(client.request("test", &Value::Null).is_err());
    }

    #[test]
    fn rpc_error_is_distinct_from_empty_result() {
        let (_root, mut client) =
            mock("q=read(); send({'id':q['id'],'error':{'code':-1,'message':'index failed'}})");
        assert!(client
            .request("test", &Value::Null)
            .unwrap_err()
            .to_string()
            .contains("index failed"));
        let (_root, mut client) = mock("q=read(); send({'id':q['id'],'result':[]})");
        assert!(
            parse_locations(&client.request("test", &Value::Null).unwrap())
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn php_configuration_excludes_hidden_caches_but_keeps_vendor_types() {
        let settings = php_configuration();
        let excludes = configuration_section(&settings, Some("intelephense.files.exclude"));
        let patterns = excludes.as_array().unwrap();
        assert!(patterns.contains(&json!("**/.*/**")));
        assert!(patterns.contains(&json!("**/node_modules/**")));
        assert!(!patterns.contains(&json!("**/vendor/**")));
        assert_eq!(
            configuration_section(&settings, Some("intelephense")),
            settings["intelephense"]
        );
        assert_eq!(
            configuration_section(&settings, Some("unknown")),
            Value::Null
        );
        assert_eq!(configuration_section(&settings, None), settings);
    }

    #[test]
    fn configuration_response_has_one_value_per_requested_section() {
        let (_root,mut client) = mock("q=read(); send({'id':q['id'],'method':'workspace/configuration','params':{'items':[{'section':'intelephense'},{'section':'files'}]}}); answer=read(); send({'id':q['id'],'result':answer['result']})");
        assert_eq!(
            client.request("test", &Value::Null).unwrap(),
            json!([null, null])
        );
    }

    #[test]
    fn oversized_message_rejected_before_allocating_body() {
        let (_root, mut client) = mock(
            r"read(); sys.stdout.buffer.write(b'Content-Length: 999999999\r\n\r\n'); sys.stdout.buffer.flush(); time.sleep(60)",
        );
        assert!(client
            .request("test", &Value::Null)
            .unwrap_err()
            .to_string()
            .contains("exceeds 16 MiB"));
    }

    #[test]
    fn shutdown_deadline_kills_unresponsive_server() {
        let (_root, client) = mock("read(); time.sleep(60)");
        let started = Instant::now();
        client.shutdown();
        assert!(started.elapsed() < Duration::from_secs(4));
    }

    #[test]
    fn legacy_trace_exposes_server_failure_and_stops_retrying_broken_transport() {
        let (root, mut client) = mock("read(); q=read(); send({'id':q['id'],'error':{'code':-32603,'message':'index failed'}})");
        let file = root.path().join("file.php");
        std::fs::write(&file, "<?php target();").unwrap();
        assert!(client.definition(&file, 0, 6).is_none());
        assert!(client.stopped);
        assert!(client.definition(&file, 0, 6).is_none());
        let mut report = crate::presentation::Report {
            text: String::new(),
            json: json!({"status":"complete"}),
        };
        crate::presentation::attach_warnings(&mut report);
        assert_eq!(report.json["status"], "partial");
        assert!(report.text.contains("index failed"));
    }

    #[test]
    fn references_exclude_declarations_and_keep_empty_results_explicit() {
        let (root, mut client) = mock("q=read(); assert q['method']=='textDocument/references'; assert q['params']['context']['includeDeclaration'] is False; send({'id':q['id'],'result':[]})");
        assert!(client
            .locations(&root.path().join("file.php"), 0, 0, true)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn duplicate_server_locations_are_deduplicated_without_collapsing_distinct_ranges() {
        let first = json!({"uri":"file:///tmp/a.php","range":{"start":{"line":1,"character":2},"end":{"line":1,"character":3}}});
        let second = json!({"uri":"file:///tmp/a.php","range":{"start":{"line":1,"character":5},"end":{"line":1,"character":6}}});
        let locations = parse_locations(&json!([second, first, first])).unwrap();
        assert_eq!(locations.len(), 2);
        assert_eq!(locations[0].character, 2);
        assert_eq!(locations[1].character, 5);
    }

    #[test]
    fn location_links_preserve_selection_range_and_multiple_candidates() {
        let locations = parse_locations(&json!([
            {"targetUri":"file:///tmp/a%20b%23%C3%A9.php","targetRange":{"start":{"line":0,"character":0},"end":{"line":50,"character":0}},"targetSelectionRange":{"start":{"line":5,"character":4},"end":{"line":5,"character":9}}},
            {"uri":"file:///tmp/other.php","range":{"start":{"line":7,"character":2},"end":{"line":8,"character":3}}}
        ])).unwrap();
        assert_eq!(locations.len(), 2);
        assert_eq!(locations[0].file, Path::new("/tmp/a b#é.php"));
        assert_eq!(
            (
                locations[0].line,
                locations[0].character,
                locations[0].end_character
            ),
            (5, 4, 9)
        );
        assert_eq!(locations[1].end_line, 8);
        assert_eq!(
            path_to_uri(&locations[0].file).unwrap(),
            "file:///tmp/a%20b%23%C3%A9.php"
        );
    }

    #[test]
    fn unicode_positions_reject_split_codepoints_and_surrogates() {
        let text = "aé😀z\r\nend\n";
        assert_eq!(byte_to_utf16(text, 0, 7).unwrap(), 4);
        assert_eq!(utf16_to_byte(text, 0, 4).unwrap(), 7);
        assert!(byte_to_utf16(text, 0, 2).is_err());
        assert!(utf16_to_byte(text, 0, 3).is_err());
        assert!(byte_to_utf16(text, 0, 9).is_err());
        assert_eq!(byte_to_utf16(text, 2, 0).unwrap(), 0);
        assert!(byte_to_utf16(text, 3, 0).is_err());
    }

    #[test]
    fn unsupported_languages_do_not_fall_back_to_typescript() {
        for language in [
            LanguageKind::Sql,
            LanguageKind::Py,
            LanguageKind::Rs,
            LanguageKind::Md,
        ] {
            assert!(server_command(language).is_err());
        }
        assert_eq!(server_command(LanguageKind::Php).unwrap().1, &["--stdio"]);
    }

    #[test]
    fn vue_uses_typescript_server_with_official_vue_plugin() {
        assert_eq!(
            server_command(LanguageKind::Vue).unwrap().0,
            "typescript-language-server"
        );
        assert_eq!(language_id(Path::new("Page.vue")), "vue");
        let options = vue_initialization_options(Path::new("/opt/@vue/language-server"));
        assert_eq!(
            options["plugins"][0],
            json!({"name":"@vue/typescript-plugin","location":"/opt/@vue/language-server",
                   "languages":["vue"],"configNamespace":"typescript"})
        );
    }

    #[test]
    fn typescript_batch_queries_require_the_semantic_server() {
        let options = typescript_initialization_options();
        assert_eq!(options["tsserver"]["useSyntaxServer"], "never");
        let vue = vue_initialization_options(Path::new("/opt/@vue/language-server"));
        assert_eq!(vue["tsserver"], options["tsserver"]);
    }

    #[test]
    #[ignore = "requires typescript-language-server and @vue/language-server on PATH"]
    fn live_vue_lsp_resolves_both_directions_on_cold_start() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/vue-lsp");
        let vue = root.join("Panel.vue");
        let service = root.join("service.ts");
        let consumer = root.join("consumer.ts");
        let mut client = LspClient::start(&root, LanguageKind::Vue).unwrap();
        // Deliberately no sleeps or warm-up: the first query must be semantic.
        assert_eq!(
            client.definition(&vue, 3, 19),
            Some((service.canonicalize().unwrap(), 1))
        );
        client.shutdown();
        let mut client = LspClient::start(&root, LanguageKind::Ts).unwrap();
        assert_eq!(
            client.definition(&consumer, 2, 11),
            Some((vue.canonicalize().unwrap(), 5))
        );
        client.shutdown();
        let trace = crate::trace::render(
            vue.to_str().unwrap(),
            &crate::trace::TraceConfig {
                max_depth: 2,
                show_all: true,
                target_symbol: Some("submit".into()),
                use_lsp: true,
            },
        )
        .unwrap();
        assert!(
            trace.contains("service.save")
                && trace.contains("service.ts")
                && trace.contains("[L2-4]"),
            "{trace}"
        );
        assert!(
            !trace.contains("unresolved") && !trace.contains("external"),
            "{trace}"
        );
    }

    #[test]
    fn vue_plugin_location_requires_the_owning_package() {
        let root = tempfile::tempdir().unwrap();
        std::fs::write(root.path().join("package.json"), r#"{"name":"other"}"#).unwrap();
        assert!(!is_vue_language_server_package(root.path()));
        std::fs::write(
            root.path().join("package.json"),
            r#"{"name":"@vue/language-server"}"#,
        )
        .unwrap();
        assert!(is_vue_language_server_package(root.path()));
    }

    #[test]
    fn fresh_document_is_sent_as_did_change() {
        let (root,mut client) = mock("opened=read(); changed=read(); q=read(); send({'id':q['id'],'result':{'method':changed['method'],'version':changed['params']['textDocument']['version'],'text':changed['params']['contentChanges'][0]['text']}})");
        let file = root.path().join("source.php");
        std::fs::write(&file, "<?php // first").unwrap();
        client.ensure_open(&file).unwrap();
        std::fs::write(&file, "<?php // edited").unwrap();
        client.ensure_open(&file).unwrap();
        let result = client.request("test", &Value::Null).unwrap();
        assert_eq!(
            result,
            json!({"method":"textDocument/didChange","version":2,"text":"<?php // edited"})
        );
    }
}
