//! Bounded agent-facing output. Overflow is exact, private, and recoverable.
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::Value;

use crate::error::XrayError;

pub struct Report {
    pub text: String,
    pub json: Value,
}

thread_local! {
    // Resolver warnings belong to this operation's bounded result, never unbounded stderr.
    static WARNINGS: std::cell::RefCell<std::collections::BTreeSet<String>> = const {
        std::cell::RefCell::new(std::collections::BTreeSet::new())
    };
}

pub fn warn(message: String) {
    WARNINGS.with(|warnings| {
        warnings.borrow_mut().insert(message);
    });
}

pub fn attach_warnings(report: &mut Report) {
    let warnings = WARNINGS.with(|warnings| std::mem::take(&mut *warnings.borrow_mut()));
    if warnings.is_empty() {
        return;
    }
    let failed = report.json.get("status").and_then(Value::as_str) == Some("failed");
    let status = if failed { "failed" } else { "partial" };
    let mut header = format!("status={status} warnings={}\n", warnings.len());
    for warning in &warnings {
        let _ = writeln!(header, "warning: {warning}");
    }
    report.text.insert_str(0, &header);
    report.json["status"] = Value::String(status.into());
    if !report.json["warnings"].is_array() {
        report.json["warnings"] = Value::Array(Vec::new());
    }
    if let Some(items) = report.json["warnings"].as_array_mut() {
        items.extend(warnings.into_iter().map(Value::String));
    }
}

pub const DEFAULT_MAX_BYTES: usize = 16 * 1024;
pub const MIN_MAX_BYTES: usize = 1024;
pub const MAX_MAX_BYTES: usize = 1024 * 1024;

pub fn bounded_text(text: &str, budget: usize) -> Result<String, XrayError> {
    if text.len() <= budget {
        return Ok(text.to_string());
    }
    let artifact = save_artifact(text.as_bytes(), "txt")?;
    // Reserve the entire footer before slicing, including worst-case digit widths.
    let footer = format!(
        "\nstatus=truncated original_bytes={} omitted_bytes={} artifact={}\n",
        text.len(),
        text.len(),
        artifact.display()
    );
    let available = budget
        .checked_sub(footer.len())
        .ok_or_else(|| XrayError::Output("overflow reference exceeds output budget".to_string()))?;
    let mut end = available.min(text.len());
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    if let Some(line_end) = text[..end].rfind('\n') {
        if line_end > end / 2 {
            end = line_end + 1;
        }
    }
    Ok(format!(
        "{}\nstatus=truncated original_bytes={} omitted_bytes={} artifact={}\n",
        &text[..end],
        text.len(),
        text.len() - end,
        artifact.display()
    ))
}

/// Keep valid structured JSON and whole leading records, never a broken JSON prefix.
pub fn bounded_json(mut value: Value, budget: usize) -> Result<String, XrayError> {
    let original = encode(&value)?;
    if original.len() <= budget {
        return Ok(original);
    }
    let artifact = save_artifact(original.as_bytes(), "json")?;
    let original_status = value.get("status").cloned().unwrap_or(Value::Null);
    let mode = value.get("mode").cloned().unwrap_or(Value::Null);
    let mut omitted = BTreeMap::<String, usize>::new();
    loop {
        value["status"] = Value::String("partial".into());
        value["truncation"] = serde_json::json!({
            "original_status": original_status,
            "original_bytes": original.len(),
            "omitted_items": omitted,
            "artifact": artifact,
        });
        let output = encode(&value)?;
        if output.len() <= budget {
            return Ok(output);
        }
        let Some((path, _)) = largest_array(&value, "") else {
            let fallback = encode(&serde_json::json!({
                "schema_version": 1, "mode": mode, "status": "partial",
                "truncation": {"original_status": original_status,
                    "original_bytes": original.len(), "artifact": artifact,
                    "reason": "individual value exceeds output budget"}
            }))?;
            return if fallback.len() <= budget {
                Ok(fallback)
            } else {
                Err(XrayError::Output(
                    "overflow reference exceeds output budget".into(),
                ))
            };
        };
        if let Some(array) = value.pointer_mut(&path).and_then(Value::as_array_mut) {
            let keep = if array.len() > 8 {
                array.len() / 2
            } else {
                array.len() - 1
            };
            *omitted.entry(path).or_default() += array.len() - keep;
            array.truncate(keep);
        }
    }
}

fn encode(value: &Value) -> Result<String, XrayError> {
    serde_json::to_string(value)
        .map(|value| value + "\n")
        .map_err(|error| XrayError::Output(error.to_string()))
}

fn largest_array(value: &Value, path: &str) -> Option<(String, usize)> {
    match value {
        Value::Array(items) if !items.is_empty() => {
            Some((path.to_string(), value.to_string().len()))
        }
        Value::Object(fields) => fields
            .iter()
            .filter(|(key, _)| key.as_str() != "truncation")
            .filter_map(|(key, value)| {
                let escaped = key.replace('~', "~0").replace('/', "~1");
                largest_array(value, &format!("{path}/{escaped}"))
            })
            .max_by_key(|(_, bytes)| *bytes),
        _ => None,
    }
}

fn save_artifact(bytes: &[u8], extension: &str) -> Result<PathBuf, XrayError> {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| XrayError::Output(error.to_string()))?
        .as_nanos();
    for attempt in 0..16 {
        let directory = std::env::temp_dir().join(format!(
            "xray-output-{}-{stamp}-{attempt}",
            std::process::id()
        ));
        let mut builder = fs::DirBuilder::new();
        #[cfg(unix)]
        {
            use std::os::unix::fs::DirBuilderExt;
            builder.mode(0o700);
        }
        match builder.create(&directory) {
            Ok(()) => {
                let path = directory.join(format!("full.{extension}"));
                let mut options = OpenOptions::new();
                options.write(true).create_new(true);
                #[cfg(unix)]
                {
                    use std::os::unix::fs::OpenOptionsExt;
                    options.mode(0o600);
                }
                let result = options
                    .open(&path)
                    .and_then(|mut file| file.write_all(bytes));
                if let Err(error) = result {
                    let _ = fs::remove_file(&path);
                    let _ = fs::remove_dir(&directory);
                    return Err(XrayError::Output(format!("save overflow: {error}")));
                }
                return Ok(path);
            }
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => return Err(XrayError::Output(format!("save overflow: {error}"))),
        }
    }
    Err(XrayError::Output(
        "cannot allocate private overflow artifact".into(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn remove_artifact(path: &std::path::Path) {
        fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }

    #[test]
    fn warnings_are_deduplicated_scoped_and_preserve_failure() {
        warn("invalid config".into());
        warn("invalid config".into());
        let mut report = Report {
            text: "failure\n".into(),
            json: serde_json::json!({"status":"failed"}),
        };
        attach_warnings(&mut report);
        assert_eq!(report.json["status"], "failed");
        assert_eq!(report.json["warnings"].as_array().unwrap().len(), 1);
        let mut next = Report {
            text: "next".into(),
            json: serde_json::json!({"status":"complete"}),
        };
        attach_warnings(&mut next);
        assert_eq!(next.text, "next");
    }

    #[test]
    fn small_text_is_unchanged() {
        assert_eq!(
            bounded_text("0 results status=complete\n", 1024).unwrap(),
            "0 results status=complete\n"
        );
    }

    #[test]
    fn text_caps_utf8_and_preserves_exact_private_overflow() {
        let original = "ação 🦀\n".repeat(1000);
        let output = bounded_text(&original, 1024).unwrap();
        assert!(output.len() <= 1024);
        let path = PathBuf::from(output.split("artifact=").nth(1).unwrap().trim());
        assert_eq!(fs::read_to_string(&path).unwrap(), original);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_eq!(
                fs::metadata(&path).unwrap().permissions().mode() & 0o777,
                0o600
            );
            assert_eq!(
                fs::metadata(path.parent().unwrap())
                    .unwrap()
                    .permissions()
                    .mode()
                    & 0o777,
                0o700
            );
        }
        remove_artifact(&path);
    }

    #[test]
    fn json_keeps_leading_records_and_reports_omissions() {
        let value = serde_json::json!({"schema_version":1,"mode":"references","status":"complete", "results":(0..100).map(|i| serde_json::json!({"line":i,"file":"app/Models/User.php"})).collect::<Vec<_>>()});
        let output = bounded_json(value.clone(), 1024).unwrap();
        assert!(output.len() <= 1024);
        let result: Value = serde_json::from_str(&output).unwrap();
        let shown = result["results"].as_array().unwrap();
        assert_eq!(shown[0]["line"], 0);
        assert_eq!(
            shown.len()
                + usize::try_from(
                    result["truncation"]["omitted_items"]["/results"]
                        .as_u64()
                        .unwrap()
                )
                .unwrap(),
            100
        );
        let path = PathBuf::from(result["truncation"]["artifact"].as_str().unwrap());
        assert_eq!(
            serde_json::from_slice::<Value>(&fs::read(&path).unwrap()).unwrap(),
            value
        );
        remove_artifact(&path);
    }

    #[test]
    fn json_preserves_failure_status_when_large_error_overflows() {
        let output = bounded_json(
            serde_json::json!({"mode":"digest", "status":"failed", "error":"x".repeat(2000)}),
            1024,
        )
        .unwrap();
        let result: Value = serde_json::from_str(&output).unwrap();
        assert_eq!(result["truncation"]["original_status"], "failed");
        assert!(output.len() <= 1024);
        remove_artifact(std::path::Path::new(
            result["truncation"]["artifact"].as_str().unwrap(),
        ));
    }
}
