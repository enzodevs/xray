mod go;
mod markdown;
mod php;
mod py;
mod rs;
mod shared;
mod sql;
mod ts;

use std::path::PathBuf;

/// Path alias configuration from tsconfig.json.
///
/// Kept at the facade layer because it is referenced by multiple command
/// pipelines (`follow`, `reverse`, `trace`) while backend-specific logic lives
/// in `resolve::ts` / `resolve::sql`.
pub struct PathConfig {
    base_url: PathBuf,
    has_explicit_base_url: bool,
    aliases: Vec<(String, Vec<String>)>,
}

impl PathConfig {
    pub(crate) fn matches_alias(&self, specifier: &str) -> bool {
        self.aliases.iter().any(|(pattern, _)| {
            pattern
                .strip_suffix('*')
                .map_or(specifier == pattern, |prefix| specifier.starts_with(prefix))
        })
    }
}

pub(crate) use go::resolve_go_import;
pub(crate) use markdown::resolve_markdown_link;
pub(crate) use php::resolve_php_import;
pub(crate) use py::resolve_py_import;
pub(crate) use rs::resolve_rs_import;
pub(crate) use sql::resolve_sql_include;
pub(crate) use ts::{collect_sources, load_path_config, resolve_import};
