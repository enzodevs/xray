use std::path::{Path, PathBuf};

use super::shared;

const SQL_EXTENSIONS: &[&str] = &["sql"];

/// Resolve a SQL include/source specifier to a local `.sql` file.
///
/// Supports relative (`./`, `../`) and bare paths (resolved relative to the
/// current file directory), which are common in SQL script include directives.
pub(crate) fn resolve_sql_include(specifier: &str, from_file: &Path) -> Option<PathBuf> {
    let parent = from_file.parent()?;
    let candidate = if Path::new(specifier).is_absolute() {
        PathBuf::from(specifier)
    } else {
        parent.join(specifier)
    };
    shared::try_extensions_with(&candidate, SQL_EXTENSIONS)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn resolve_sql_include_relative_finds_sql_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.sql");
        fs::write(&entry, "").unwrap();
        let scripts = dir.path().join("scripts");
        fs::create_dir(&scripts).unwrap();
        let target = scripts.join("seed.sql");
        fs::write(&target, "").unwrap();

        let result = resolve_sql_include("./scripts/seed", &entry);
        assert_eq!(result, Some(target));
    }

    #[test]
    fn resolve_sql_include_bare_path_is_relative_to_file() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("main.sql");
        fs::write(&entry, "").unwrap();
        let target = dir.path().join("bootstrap.sql");
        fs::write(&target, "").unwrap();

        let result = resolve_sql_include("bootstrap.sql", &entry);
        assert_eq!(result, Some(target));
    }
}
