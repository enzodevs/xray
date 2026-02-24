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
    aliases: Vec<(String, Vec<String>)>,
}

pub(crate) use sql::resolve_sql_include;
pub(crate) use ts::{collect_sources, load_path_config, resolve_import};
