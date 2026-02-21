use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::error::XrayError;
use crate::model::FileSummary;
use crate::output::FileDigest;
use crate::resolve::{self, PathConfig};

/// Configuration for follow mode.
pub struct FollowConfig {
    pub max_depth: usize,
    pub show_all: bool,
}

/// A node in the dependency tree.
struct DepNode {
    summary: FileSummary,
    children: Vec<DepNode>,
    omitted: Vec<String>,
}

/// Path segments that indicate UI primitive / noise files.
const NOISE_SEGMENTS: &[&str] = &["/ui/", "/components/ui/", "/primitives/"];

/// Specific file suffixes considered noise.
const NOISE_SUFFIXES: &[&str] = &[
    "/lib/utils.ts",
    "/lib/utils.js",
    "/utils/cn.ts",
    "/utils/cn.js",
];

/// Process a file in follow mode: full digest for entry, tree of summaries.
pub fn run(entry_path: &str, config: &FollowConfig) -> Result<(), XrayError> {
    let entry = std::fs::canonicalize(Path::new(entry_path)).map_err(|e| XrayError::Io {
        path: entry_path.to_string(),
        source: e,
    })?;

    let path_config = entry.parent().and_then(resolve::load_path_config);
    let digest = FileDigest::from_path(&entry)?;

    let sources = resolve::collect_sources(&digest.symbols.imports, &digest.symbols.reexports);
    print!("{digest}");

    if sources.is_empty() {
        return Ok(());
    }

    let mut visited = HashSet::new();
    visited.insert(entry.clone());

    let mut children = Vec::new();
    let mut omitted = Vec::new();

    for specifier in &sources {
        let Some(resolved) = resolve::resolve_import(specifier, &entry, path_config.as_ref())
        else {
            continue;
        };

        let Ok(canonical) = resolved.canonicalize() else {
            continue;
        };
        if !visited.insert(canonical.clone()) {
            continue;
        }

        if !config.show_all && is_noise_path(&canonical) {
            omitted.push(noise_label(&canonical));
            continue;
        }

        match build_subtree(&canonical, 1, config, path_config.as_ref(), &mut visited) {
            Ok(node) => children.push(node),
            Err(e) => eprintln!("xray: {e}"),
        }
    }

    if !children.is_empty() || !omitted.is_empty() {
        println!("│");
        render_tree(&children, &omitted, "");
    }

    Ok(())
}

/// Recursively build a dependency subtree.
fn build_subtree(
    path: &Path,
    depth: usize,
    config: &FollowConfig,
    path_config: Option<&PathConfig>,
    visited: &mut HashSet<PathBuf>,
) -> Result<DepNode, XrayError> {
    let digest = FileDigest::from_path(path)?;
    let summary = digest.summarize();

    let mut children = Vec::new();
    let mut omitted = Vec::new();

    if depth < config.max_depth {
        let sources =
            resolve::collect_sources(&digest.symbols.imports, &digest.symbols.reexports);

        for specifier in &sources {
            let Some(resolved) = resolve::resolve_import(specifier, path, path_config) else {
                continue;
            };

            let Ok(canonical) = resolved.canonicalize() else {
                continue;
            };
            if !visited.insert(canonical.clone()) {
                continue;
            }

            if !config.show_all && is_noise_path(&canonical) {
                omitted.push(noise_label(&canonical));
                continue;
            }

            match build_subtree(&canonical, depth + 1, config, path_config, visited) {
                Ok(node) => children.push(node),
                Err(e) => eprintln!("xray: {e}"),
            }
        }
    }

    Ok(DepNode {
        summary,
        children,
        omitted,
    })
}

/// Render the dependency tree with box-drawing characters.
fn render_tree(children: &[DepNode], omitted: &[String], prefix: &str) {
    let has_omitted = !omitted.is_empty();

    for (i, child) in children.iter().enumerate() {
        let is_last = i == children.len() - 1 && !has_omitted;
        let connector = if is_last { "└── " } else { "├── " };
        let continuation = if is_last { "    " } else { "│   " };

        let summary_str = child.summary.to_string();
        let mut lines = summary_str.lines();
        if let Some(first) = lines.next() {
            println!("{prefix}{connector}{first}");
        }
        for line in lines {
            println!("{prefix}{continuation}{line}");
        }

        if !child.children.is_empty() || !child.omitted.is_empty() {
            println!("{prefix}{continuation}│");
            let sub_prefix = format!("{prefix}{continuation}");
            render_tree(&child.children, &child.omitted, &sub_prefix);
        }
    }

    if has_omitted {
        let label = if omitted.len() == 1 {
            format!("[1 UI primitive omitted: {}]", omitted[0])
        } else {
            format!(
                "[{} UI primitives omitted: {}]",
                omitted.len(),
                omitted.join(", ")
            )
        };
        println!("{prefix}└── {label}");
    }
}

/// Check if a resolved path matches noise patterns.
pub(crate) fn is_noise_path(path: &Path) -> bool {
    let rel = crate::util::relative_path(path);
    is_noise_segment(&rel)
}

/// Check noise patterns against a relative path string. Testable without filesystem.
fn is_noise_segment(rel: &str) -> bool {
    use std::borrow::Cow;

    let normalized: Cow<'_, str> = if cfg!(windows) {
        Cow::Owned(rel.replace('\\', "/"))
    } else {
        Cow::Borrowed(rel)
    };
    let prefixed = format!("/{normalized}");

    NOISE_SEGMENTS
        .iter()
        .any(|seg| prefixed.contains(seg))
        || NOISE_SUFFIXES
            .iter()
            .any(|suf| prefixed.ends_with(suf))
}

/// Extract a short label from a noise path (just the filename stem).
fn noise_label(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("?")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_noise_segment_matches_ui_directory() {
        assert!(is_noise_segment("src/components/ui/button.tsx"));
    }

    #[test]
    fn is_noise_segment_matches_primitives() {
        assert!(is_noise_segment("src/primitives/input.tsx"));
    }

    #[test]
    fn is_noise_segment_ignores_normal_components() {
        assert!(!is_noise_segment("src/components/chat.tsx"));
    }

    #[test]
    fn is_noise_segment_matches_utils() {
        assert!(is_noise_segment("src/lib/utils.ts"));
    }

    #[test]
    fn is_noise_segment_ignores_non_trivial_utils() {
        assert!(!is_noise_segment("src/utils/format-date.ts"));
    }

    #[test]
    fn noise_label_extracts_stem() {
        let path = Path::new("/project/src/ui/button.tsx");
        assert_eq!(noise_label(path), "button");
    }
}
