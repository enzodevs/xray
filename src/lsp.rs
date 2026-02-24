use std::collections::HashSet;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

use crate::error::XrayError;

/// Minimal LSP client that communicates with `typescript-language-server` via stdio.
pub struct LspClient {
    child: Child,
    stdin: BufWriter<std::process::ChildStdin>,
    stdout: BufReader<std::process::ChildStdout>,
    next_id: i64,
    opened_files: HashSet<PathBuf>,
}

impl LspClient {
    /// Spawn the language server and perform the initialize handshake.
    pub fn start(root: &Path) -> Result<Self, XrayError> {
        let mut child = Command::new("typescript-language-server")
            .arg("--stdio")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| XrayError::Lsp(format!("spawn failed: {e}")))?;

        let stdin = BufWriter::new(
            child
                .stdin
                .take()
                .ok_or_else(|| XrayError::Lsp("no stdin".to_string()))?,
        );
        let stdout = BufReader::new(
            child
                .stdout
                .take()
                .ok_or_else(|| XrayError::Lsp("no stdout".to_string()))?,
        );

        let mut client = Self {
            child,
            stdin,
            stdout,
            next_id: 1,
            opened_files: HashSet::new(),
        };

        let root_uri = format!("file://{}", root.display());
        let init_params = serde_json::json!({
            "processId": std::process::id(),
            "rootUri": root_uri,
            "capabilities": {},
            "rootPath": root.to_string_lossy(),
        });
        let _resp = client.request("initialize", &init_params)?;
        client.notify("initialized", &serde_json::json!({}))?;

        Ok(client)
    }

    /// Ask for the definition location of the symbol at the given position.
    ///
    /// `line` and `col` are 0-based (LSP convention).
    /// Returns `(absolute_path, 0-based line)` on success.
    pub fn definition(&mut self, file: &Path, line: usize, col: usize) -> Option<(PathBuf, usize)> {
        self.ensure_open(file).ok()?;

        let uri = path_to_uri(file);
        let params = serde_json::json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": col },
        });

        let result = self.request("textDocument/definition", &params).ok()?;
        parse_location(&result)
    }

    /// Gracefully shut down the language server.
    pub fn shutdown(mut self) {
        let _ = self.request("shutdown", &serde_json::json!(null));
        let _ = self.notify("exit", &serde_json::json!(null));
        let _ = self.child.wait();
    }

    /// Open a file and its local imports, then wait for the type graph.
    ///
    /// Eagerly opens imported files so the tsserver has the full type
    /// context available when we start querying definitions. Built-in
    /// types resolve immediately; user-defined types need the imported
    /// modules to be loaded by the server.
    pub fn warm_up(&mut self, file: &Path, import_paths: &[PathBuf]) {
        let _ = self.ensure_open(file);
        for dep in import_paths {
            let _ = self.ensure_open(dep);
        }
        std::thread::sleep(std::time::Duration::from_secs(2));
    }

    /// Send `textDocument/didOpen` if the file hasn't been opened yet.
    ///
    /// Sends a hover request at (0,0) and waits for the response, which
    /// forces the server to process the `didOpen` before returning.
    fn ensure_open(&mut self, file: &Path) -> Result<(), XrayError> {
        let canonical = std::fs::canonicalize(file).unwrap_or_else(|_| file.to_path_buf());
        if self.opened_files.contains(&canonical) {
            return Ok(());
        }

        let text = std::fs::read_to_string(&canonical)
            .map_err(|e| XrayError::Lsp(format!("read {}: {e}", canonical.display())))?;
        let uri = path_to_uri(&canonical);
        let lang_id = language_id(&canonical);

        let params = serde_json::json!({
            "textDocument": {
                "uri": uri,
                "languageId": lang_id,
                "version": 1,
                "text": text,
            }
        });
        self.notify("textDocument/didOpen", &params)?;

        let hover_params = serde_json::json!({
            "textDocument": { "uri": &uri },
            "position": { "line": 0, "character": 0 },
        });
        let _ = self.request("textDocument/hover", &hover_params);

        self.opened_files.insert(canonical);
        Ok(())
    }

    /// Send a JSON-RPC request and wait for the matching response.
    fn request(
        &mut self,
        method: &str,
        params: &serde_json::Value,
    ) -> Result<serde_json::Value, XrayError> {
        let id = self.next_id;
        self.next_id += 1;

        let msg = serde_json::json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params,
        });
        self.send(&msg)?;

        // Read responses, handling server requests and skipping notifications.
        loop {
            let body = self.read_message()?;
            let parsed: serde_json::Value =
                serde_json::from_str(&body).map_err(|e| XrayError::Lsp(format!("json: {e}")))?;

            // Our response.
            if parsed.get("id").and_then(serde_json::Value::as_i64) == Some(id) {
                if let Some(err) = parsed.get("error") {
                    return Err(XrayError::Lsp(format!("server error: {err}")));
                }
                return Ok(parsed
                    .get("result")
                    .cloned()
                    .unwrap_or(serde_json::Value::Null));
            }

            // Server-initiated request: respond with null to prevent deadlock.
            if parsed.get("id").is_some() && parsed.get("method").is_some() {
                let response = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": parsed["id"],
                    "result": null,
                });
                self.send(&response)?;
            }
            // Otherwise it's a notification — skip it.
        }
    }

    /// Send a JSON-RPC notification (no response expected).
    fn notify(&mut self, method: &str, params: &serde_json::Value) -> Result<(), XrayError> {
        let msg = serde_json::json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        });
        self.send(&msg)
    }

    /// Write a JSON-RPC message with Content-Length header.
    fn send(&mut self, msg: &serde_json::Value) -> Result<(), XrayError> {
        let body =
            serde_json::to_string(msg).map_err(|e| XrayError::Lsp(format!("serialize: {e}")))?;
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin
            .write_all(header.as_bytes())
            .and_then(|()| self.stdin.write_all(body.as_bytes()))
            .and_then(|()| self.stdin.flush())
            .map_err(|e| XrayError::Lsp(format!("write: {e}")))
    }

    /// Read one JSON-RPC message from stdout (Content-Length framing).
    fn read_message(&mut self) -> Result<String, XrayError> {
        let mut content_length: Option<usize> = None;

        // Read headers until empty line.
        loop {
            let mut line = String::new();
            self.stdout
                .read_line(&mut line)
                .map_err(|e| XrayError::Lsp(format!("read header: {e}")))?;

            if line.is_empty() {
                return Err(XrayError::Lsp("server closed stdout".to_string()));
            }

            let trimmed = line.trim();
            if trimmed.is_empty() {
                break;
            }

            if let Some(val) = trimmed.strip_prefix("Content-Length: ") {
                content_length = val.parse().ok();
            }
        }

        let len =
            content_length.ok_or_else(|| XrayError::Lsp("missing Content-Length".to_string()))?;

        let mut buf = vec![0u8; len];
        self.stdout
            .read_exact(&mut buf)
            .map_err(|e| XrayError::Lsp(format!("read body: {e}")))?;

        String::from_utf8(buf).map_err(|e| XrayError::Lsp(format!("utf8: {e}")))
    }
}

/// Convert a file path to a `file://` URI.
fn path_to_uri(path: &Path) -> String {
    let abs = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    format!("file://{}", abs.display())
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Map file extension to LSP `languageId`.
fn language_id(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some("ts" | "mts") => "typescript",
        Some("tsx") => "typescriptreact",
        Some("jsx") => "javascriptreact",
        _ => "javascript",
    }
}

/// Parse a definition response into `(path, 0-based line)`.
fn parse_location(result: &serde_json::Value) -> Option<(PathBuf, usize)> {
    // Response can be a single Location, an array of Locations, or null.
    let loc = if result.is_array() {
        result.get(0)?
    } else if result.get("uri").is_some() {
        result
    } else {
        return None;
    };

    let uri = loc.get("uri")?.as_str()?;
    let path = uri.strip_prefix("file://")?;
    let line = loc.get("range")?.get("start")?.get("line")?.as_u64()?;

    let Ok(line_usize) = usize::try_from(line) else {
        return None;
    };
    Some((PathBuf::from(path), line_usize))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_to_uri_absolute() {
        let uri = path_to_uri(Path::new("/tmp/test.ts"));
        assert!(uri.starts_with("file:///"));
        assert!(uri.ends_with("test.ts"));
    }

    #[test]
    fn language_id_mapping() {
        assert_eq!(language_id(Path::new("a.ts")), "typescript");
        assert_eq!(language_id(Path::new("a.mts")), "typescript");
        assert_eq!(language_id(Path::new("a.tsx")), "typescriptreact");
        assert_eq!(language_id(Path::new("a.jsx")), "javascriptreact");
        assert_eq!(language_id(Path::new("a.js")), "javascript");
        assert_eq!(language_id(Path::new("a.mjs")), "javascript");
        assert_eq!(language_id(Path::new("a.cjs")), "javascript");
    }

    #[test]
    fn parse_location_array() {
        let json = serde_json::json!([{
            "uri": "file:///src/service.ts",
            "range": {
                "start": { "line": 42, "character": 2 },
                "end": { "line": 42, "character": 10 },
            }
        }]);
        let (path, line) = parse_location(&json).unwrap();
        assert_eq!(path, PathBuf::from("/src/service.ts"));
        assert_eq!(line, 42);
    }

    #[test]
    fn parse_location_single() {
        let json = serde_json::json!({
            "uri": "file:///src/service.ts",
            "range": {
                "start": { "line": 10, "character": 0 },
                "end": { "line": 10, "character": 5 },
            }
        });
        let (path, line) = parse_location(&json).unwrap();
        assert_eq!(path, PathBuf::from("/src/service.ts"));
        assert_eq!(line, 10);
    }

    #[test]
    fn parse_location_null_returns_none() {
        assert!(parse_location(&serde_json::Value::Null).is_none());
    }

    #[test]
    fn parse_location_empty_array_returns_none() {
        assert!(parse_location(&serde_json::json!([])).is_none());
    }
}
