mod go;
mod markdown;
mod php;
mod py;
mod rs;
mod shared;
mod sql;
mod ts;

use std::sync::Arc;

/// Path alias configuration from tsconfig.json.
///
/// Kept at the facade layer because it is referenced by multiple command
/// pipelines (`follow`, `reverse`, `trace`) while backend-specific logic lives
/// in `resolve::ts` / `resolve::sql`.
#[derive(Clone)]
pub struct PathConfig {
    config: Arc<oxc_resolver::TsConfig>,
}

impl PathConfig {
    pub(crate) fn matches_alias(&self, specifier: &str) -> bool {
        self.config
            .compiler_options
            .paths
            .as_ref()
            .is_some_and(|paths| {
                paths.keys().any(|pattern| {
                    pattern
                        .split_once('*')
                        .map_or(specifier == pattern, |(prefix, suffix)| {
                            specifier.len() >= prefix.len() + suffix.len()
                                && specifier.starts_with(prefix)
                                && specifier.ends_with(suffix)
                        })
                })
            })
    }
}

pub(crate) use go::{go_package_files, resolve_go_import};
pub(crate) use markdown::resolve_markdown_link;
pub(crate) use php::{resolve_php_import, resolve_php_import_with_paths, PhpPaths};
pub(crate) use py::resolve_py_import;
pub(crate) use rs::resolve_rs_import;
pub(crate) use sql::resolve_sql_include;
pub(crate) use ts::{collect_sources, load_path_config, resolve_import};
