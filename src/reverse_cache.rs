//! Private, disposable parse facts only: Git visibility and resolved edges are never cached.
use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use serde::{Deserialize, Serialize};

use crate::error::XrayError;
use crate::lang::LanguageKind;
use crate::{model::FileContent, parser};

const FORMAT: u32 = 1;
const MAX_BYTES: u64 = 32 * 1024 * 1024;
const MAX_ENTRIES: usize = 20_000;

#[derive(Serialize, Deserialize)]
pub(super) struct Facts {
    pub specifiers: Vec<String>,
    pub sql_calls: Vec<String>,
    pub recovered: bool,
    pub lines: usize,
}

impl Facts {
    fn parse(path: &Path) -> Result<Self, XrayError> {
        let parsed = parser::parse_file(path)?;
        let sql_calls = if parsed.language_kind == LanguageKind::Sql {
            match parsed.extract_content() {
                FileContent::Code(symbols) => symbols
                    .internals
                    .into_iter()
                    .flat_map(|s| s.calls)
                    .filter_map(|call| call.strip_prefix("fn:").map(str::to_owned))
                    .collect(),
                FileContent::Markdown(_) => Vec::new(),
            }
        } else {
            Vec::new()
        };
        Ok(Self {
            specifiers: parsed.diagnostic_specifiers(),
            sql_calls,
            recovered: parsed.has_syntax_errors(),
            lines: parsed.source.lines().count(),
        })
    }
}

#[derive(PartialEq, Eq, Serialize, Deserialize)]
struct Stamp {
    size: u64,
    modified: SystemTime,
    // Detect replacement and edits that preserve mtime on Unix, including chmod.
    #[cfg(unix)]
    identity: (u64, u64, i64, i64),
}

impl Stamp {
    fn read(path: &Path) -> Option<Self> {
        #[cfg(unix)]
        use std::os::unix::fs::MetadataExt;
        let metadata = fs::metadata(path).ok()?;
        Some(Self {
            size: metadata.len(),
            modified: metadata.modified().ok()?,
            #[cfg(unix)]
            identity: (
                metadata.dev(),
                metadata.ino(),
                metadata.ctime(),
                metadata.ctime_nsec(),
            ),
        })
    }
}

#[derive(Serialize, Deserialize)]
struct Entry {
    stamp: Stamp,
    facts: Facts,
}

#[derive(PartialEq, Eq, Serialize, Deserialize)]
struct Binary {
    path: PathBuf,
    stamp: Stamp,
}

#[derive(Serialize, Deserialize)]
struct Snapshot {
    format: u32,
    binary: Binary,
    entries: BTreeMap<PathBuf, Entry>,
}

pub(super) struct Cache {
    directory: Option<PathBuf>,
    snapshot: Option<Snapshot>,
    // Only retain facts used in this query, bounding growth across deleted files/projects.
    fresh: BTreeMap<PathBuf, Entry>,
    pub hits: usize,
    pub misses: usize,
}

impl Cache {
    pub fn open() -> Self {
        Self::at(default_directory())
    }

    pub(super) fn at(directory: Option<PathBuf>) -> Self {
        let directory = directory.filter(|path| private_directory(path).is_ok());
        let binary = std::env::current_exe().ok().and_then(|path| {
            Some(Binary {
                stamp: Stamp::read(&path)?,
                path,
            })
        });
        let snapshot = binary.map(|binary| {
            directory
                .as_ref()
                .and_then(|dir| read_snapshot(&dir.join("facts.json")))
                .filter(|saved| saved.format == FORMAT && saved.binary == binary)
                .unwrap_or_else(|| Snapshot {
                    format: FORMAT,
                    binary,
                    entries: BTreeMap::new(),
                })
        });
        Self {
            directory,
            snapshot,
            fresh: BTreeMap::new(),
            hits: 0,
            misses: 0,
        }
    }

    pub fn facts(&mut self, path: &Path) -> Result<&Facts, XrayError> {
        let stamp = Stamp::read(path);
        let saved = self.snapshot.as_mut().and_then(|s| s.entries.remove(path));
        let entry = if let Some(saved) = saved.filter(|saved| Some(&saved.stamp) == stamp.as_ref())
        {
            self.hits += 1;
            saved
        } else {
            self.misses += 1;
            let facts = Facts::parse(path)?;
            // Don't persist facts if the source changed while being parsed.
            let after = Stamp::read(path);
            let stable = stamp == after;
            let entry = Entry {
                stamp: after.unwrap_or(Stamp {
                    size: 0,
                    modified: SystemTime::UNIX_EPOCH,
                    #[cfg(unix)]
                    identity: (0, 0, 0, 0),
                }),
                facts,
            };
            if !stable {
                self.directory = None;
            }
            entry
        };
        // The last slot is disposable scratch space once the entry limit is reached.
        if self.fresh.len() >= MAX_ENTRIES {
            self.fresh.pop_first();
        }
        Ok(&self.fresh.entry(path.to_path_buf()).or_insert(entry).facts)
    }

    pub fn save(mut self) {
        let (Some(directory), Some(mut snapshot)) = (self.directory.take(), self.snapshot.take())
        else {
            return;
        };
        snapshot.entries = self.fresh;
        let Ok(bytes) = serde_json::to_vec(&snapshot) else {
            return;
        };
        if bytes.len() as u64 > MAX_BYTES {
            return;
        }
        // One lock and one temporary file bound disk use even after crashes.
        // A competing writer may skip saving; parse facts are only an optimization.
        let Some(_lock) = writer_lock(&directory) else {
            return;
        };
        let temporary = directory.join("facts.tmp");
        let _ = fs::remove_file(&temporary);
        // create_new + rename avoids following links and readers seeing partial JSON.
        let mut options = OpenOptions::new();
        options.write(true).create_new(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            options.mode(0o600);
        }
        let Ok(mut file) = options.open(&temporary) else {
            return;
        };
        if file.write_all(&bytes).is_ok() {
            let _ = fs::rename(&temporary, directory.join("facts.json"));
        }
        let _ = fs::remove_file(temporary);
    }
}

fn writer_lock(directory: &Path) -> Option<File> {
    #[cfg(unix)]
    {
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(false)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
            .open(directory.join("write.lock"))
            .ok()?;
        let metadata = file.metadata().ok()?;
        // SAFETY: geteuid has no preconditions; flock receives a live descriptor.
        if !metadata.is_file()
            || metadata.mode() & 0o777 != 0o600
            || metadata.uid() != unsafe { libc::geteuid() }
            || unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) } != 0
        {
            return None;
        }
        Some(file)
    }
    #[cfg(not(unix))]
    {
        let _ = directory;
        None
    }
}

fn default_directory() -> Option<PathBuf> {
    // On platforms where we cannot verify owner-only access, simply parse normally.
    if !cfg!(unix) {
        return None;
    }
    std::env::var_os("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .filter(|path| path.is_absolute())
        .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache")))
        .map(|base| base.join("xray-who"))
}

fn private_directory(path: &Path) -> std::io::Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::{DirBuilderExt, MetadataExt};
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        match fs::DirBuilder::new().mode(0o700).create(path) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => return Err(error),
        }
        let metadata = fs::symlink_metadata(path)?;
        // SAFETY: geteuid has no preconditions and does not mutate memory.
        let uid = unsafe { libc::geteuid() };
        if metadata.is_dir() && metadata.uid() == uid && metadata.mode() & 0o777 == 0o700 {
            return Ok(());
        }
    }
    Err(std::io::Error::other("cache directory is not private"))
}

fn read_snapshot(path: &Path) -> Option<Snapshot> {
    let mut options = OpenOptions::new();
    options.read(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK);
    }
    let file: File = options.open(path).ok()?;
    let metadata = file.metadata().ok()?;
    if !metadata.is_file() || metadata.len() > MAX_BYTES {
        return None;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        // SAFETY: geteuid has no preconditions and does not mutate memory.
        if metadata.uid() != unsafe { libc::geteuid() } || metadata.mode() & 0o777 != 0o600 {
            return None;
        }
    }
    let mut bytes = Vec::new();
    file.take(MAX_BYTES + 1).read_to_end(&mut bytes).ok()?;
    if bytes.len() as u64 > MAX_BYTES {
        return None;
    }
    let snapshot: Snapshot = serde_json::from_slice(&bytes).ok()?;
    (snapshot.entries.len() <= MAX_ENTRIES).then_some(snapshot)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn warm_facts_preserve_recovery_and_invalidate_edits() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("consumer.ts");
        let cache_path = dir.path().join("cache");
        fs::write(&path, "import './target';\nconst broken = ;").unwrap();
        let mut cache = Cache::at(Some(cache_path.clone()));
        assert!(cache.facts(&path).unwrap().recovered);
        assert_eq!(cache.misses, 1);
        cache.save();
        let mut cache = Cache::at(Some(cache_path.clone()));
        assert!(cache.facts(&path).unwrap().recovered);
        assert_eq!(cache.hits, 1);
        cache.save();
        fs::write(&path, "import './other';").unwrap();
        let mut cache = Cache::at(Some(cache_path));
        assert_eq!(cache.facts(&path).unwrap().specifiers, ["./other"]);
        assert_eq!(cache.misses, 1);
    }

    #[test]
    fn corrupt_or_incompatible_cache_recomputes() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("source.ts");
        let cache_path = dir.path().join("cache");
        fs::write(&path, "export {};").unwrap();
        let mut cache = Cache::at(Some(cache_path.clone()));
        cache.facts(&path).unwrap();
        cache.save();
        let file = cache_path.join("facts.json");
        let mut snapshot = read_snapshot(&file).unwrap();
        snapshot.format += 1;
        fs::write(&file, serde_json::to_vec(&snapshot).unwrap()).unwrap();
        let mut cache = Cache::at(Some(cache_path.clone()));
        cache.facts(&path).unwrap();
        assert_eq!(cache.misses, 1);
        fs::write(&file, "{broken").unwrap();
        let mut cache = Cache::at(Some(cache_path));
        cache.facts(&path).unwrap();
        assert_eq!(cache.misses, 1);
    }

    #[cfg(unix)]
    #[test]
    fn cache_rejects_symlinks_and_public_directory() {
        use std::os::unix::fs::{symlink, PermissionsExt};
        let dir = tempfile::tempdir().unwrap();
        let cache_path = dir.path().join("cache");
        fs::create_dir(&cache_path).unwrap();
        fs::set_permissions(&cache_path, fs::Permissions::from_mode(0o755)).unwrap();
        assert!(Cache::at(Some(cache_path.clone())).directory.is_none());
        fs::set_permissions(&cache_path, fs::Permissions::from_mode(0o700)).unwrap();
        let link = dir.path().join("link");
        symlink(&cache_path, &link).unwrap();
        assert!(Cache::at(Some(link)).directory.is_none());
        symlink(dir.path().join("missing"), cache_path.join("facts.json")).unwrap();
        assert!(read_snapshot(&cache_path.join("facts.json")).is_none());
    }
}
