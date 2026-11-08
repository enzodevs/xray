//! SQL client include commands are not SQL identifiers. Recognize them only at a
//! statement boundary, outside comments and quoted/dollar-quoted bodies.

#[derive(Default)]
struct Context {
    statement: bool,
    block_comments: usize,
    quote: Option<String>,
}

pub(super) fn extract(src: &[u8]) -> Vec<String> {
    let text = String::from_utf8_lossy(src);
    let mut context = Context::default();
    let mut sources = Vec::new();
    for line in text.lines() {
        if let Some(path) = context.scan(line) {
            if !sources.contains(&path) {
                sources.push(path);
            }
        }
    }
    sources
}

impl Context {
    fn scan(&mut self, line: &str) -> Option<String> {
        let bytes = line.as_bytes();
        let mut i = 0;
        let mut leading = true;
        let mut include = None;
        while i < bytes.len() {
            let rest = &bytes[i..];
            if self.block_comments > 0 {
                if rest.starts_with(b"/*") {
                    self.block_comments += 1;
                    i += 2;
                } else if rest.starts_with(b"*/") {
                    self.block_comments -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
                continue;
            }
            if let Some(quote) = &self.quote {
                if rest.starts_with(quote.as_bytes()) {
                    i += quote.len();
                    if quote.len() == 1 && bytes[i..].starts_with(quote.as_bytes()) {
                        i += 1; // SQL doubled quote escape
                    } else {
                        self.quote = None;
                    }
                } else if rest[0] == b'\\' && quote.len() == 1 {
                    i = (i + 2).min(bytes.len());
                } else {
                    i += 1;
                }
                continue;
            }
            if rest.starts_with(b"--") {
                break;
            }
            if rest.starts_with(b"/*") {
                self.block_comments = 1;
                i += 2;
                continue;
            }
            if rest[0].is_ascii_whitespace() {
                i += 1;
                continue;
            }
            if leading && !self.statement {
                if let Some((path, tail)) = line.get(i..).and_then(include_command) {
                    include = Some(path);
                    i = line.len() - tail.len();
                    leading = false;
                    continue; // retain trailing block-comment state across lines
                }
                // Other psql commands do not start a SQL statement.
                if rest[0] == b'\\' {
                    return None;
                }
            }
            leading = false;
            self.statement = rest[0] != b';';
            match rest[0] {
                b'\'' | b'"' | b'`' => self.quote = Some(char::from(rest[0]).to_string()),
                b'$' => {
                    if let Some(end) = rest[1..].iter().position(|byte| *byte == b'$') {
                        let tag = &rest[1..=end];
                        if tag
                            .iter()
                            .all(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
                        {
                            self.quote = line.get(i..i + end + 2).map(str::to_string);
                            i += end + 1;
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }
        include
    }
}

fn include_command(line: &str) -> Option<(String, &str)> {
    let rest = if let Some(rest) = line
        .strip_prefix("\\ir")
        .or_else(|| line.strip_prefix("\\i"))
    {
        if !rest.starts_with(char::is_whitespace) {
            return None;
        }
        rest
    } else if line
        .get(..6)
        .is_some_and(|prefix| prefix.eq_ignore_ascii_case("source"))
    {
        let rest = &line[6..];
        if !rest.starts_with(char::is_whitespace) {
            return None;
        }
        rest
    } else {
        let rest = line.strip_prefix("@@").or_else(|| line.strip_prefix('@'))?;
        let (path, tail) = include_path(rest)?;
        return std::path::Path::new(&path)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("sql"))
            .then_some((path, tail));
    };
    include_path(rest)
}

fn include_path(rest: &str) -> Option<(String, &str)> {
    let rest = rest.trim_start();
    let (path, tail) =
        if let Some(quote) = rest.chars().next().filter(|ch| matches!(ch, '\'' | '"')) {
            let end = rest[1..].find(quote)? + 1;
            (&rest[1..end], &rest[end + 1..])
        } else {
            let end = rest
                .find(|ch: char| ch.is_whitespace() || ch == ';')
                .unwrap_or(rest.len());
            (&rest[..end], &rest[end..])
        };
    let tail = tail.trim_start();
    let tail = tail.strip_prefix(';').unwrap_or(tail).trim_start();
    if path.is_empty()
        || path.ends_with(',')
        || !(tail.is_empty() || tail.starts_with("--") || tail.starts_with("/*"))
    {
        return None;
    }
    Some((path.to_string(), tail))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_columns_and_quoted_bodies_are_not_includes() {
        let sql = r"CREATE TABLE records (
 source VARCHAR NOT NULL,
 source_path TEXT
);
SELECT '
SOURCE fake.sql;
';
DO $body$
SOURCE also_fake.sql;
$body$;
/* outer /* nested */
SOURCE comment.sql;
*/
SOURCE real.sql;
";
        assert_eq!(extract(sql.as_bytes()), vec!["real.sql"]);
    }

    #[test]
    fn preserves_unicode_spaces_and_comment_characters_in_paths() {
        assert_eq!(
            extract(
                "\\i 'esquema--ação.sql'\nSOURCE \"a b.sql\"; -- trailing\n@@deploy.sql\n"
                    .as_bytes()
            ),
            vec!["esquema--ação.sql", "a b.sql", "deploy.sql"]
        );
    }

    #[test]
    fn includes_retain_trailing_multiline_comment_state() {
        assert_eq!(
            extract(b"SOURCE real.sql; /* trailing\nSOURCE ignored.sql;\n*/\n\\i next.sql\n"),
            vec!["real.sql", "next.sql"]
        );
    }

    #[test]
    fn multiline_column_and_comments_do_not_start_client_commands() {
        let sql = b"CREATE TABLE records (\n source VARCHAR\n);\n/* leading */ SOURCE real.sql;\n\\if true\n\\i more.sql\n\\endif\n";
        assert_eq!(extract(sql), vec!["real.sql", "more.sql"]);
    }
}
