use std::path::{Path, PathBuf};

pub(super) fn try_extensions_with(base: &Path, exts: &[&str]) -> Option<PathBuf> {
    // If the path already has a known extension, check it directly first.
    if let Some(ext) = base.extension().and_then(|e| e.to_str()) {
        if exts.contains(&ext) && base.is_file() {
            return Some(base.to_path_buf());
        }
    }

    // Substitute explicit ecosystem extensions before appending another one.
    // TypeScript projects commonly emit `./user.js` imports that resolve back
    // to `./user.ts`; checking appended names first could incorrectly prefer
    // an unrelated `user.js.ts` file.
    if base
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| exts.contains(&ext))
    {
        for ext in exts {
            let candidate = base.with_extension(ext);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }

    for ext in exts {
        // Append rather than replace: `./user.service` must resolve to
        // `./user.service.ts`, not `./user.ts`.
        let mut candidate = base.as_os_str().to_os_string();
        candidate.push(".");
        candidate.push(ext);
        let candidate = PathBuf::from(candidate);
        if candidate.is_file() {
            return Some(candidate);
        }
    }

    for ext in exts {
        let candidate = base.join("index").with_extension(ext);
        if candidate.is_file() {
            return Some(candidate);
        }
    }

    None
}
