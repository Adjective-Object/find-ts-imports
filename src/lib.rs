extern crate lazy_static;
extern crate regex;

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::path::Path;
use std::str;

#[cfg(feature = "serde")]
#[macro_use]
extern crate serde;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SourceFileImportData {
    /**
     * Map from import path to the name that was imported
     */
    pub imports: HashMap<String, Option<HashSet<String>>>,
}

fn parse_imported_names(name_stub: &str) -> HashSet<String> {
    if !name_stub.starts_with('{') && !name_stub.ends_with('}') {
        let mut s = HashSet::<String>::new();
        s.insert(String::from(if name_stub == "*" {
            name_stub
        } else {
            "default"
        }));
        return s;
    } else {
        let trimmed_stub = &name_stub[1..name_stub.len() - 1];
        return HashSet::<String>::from_iter(
            trimmed_stub
                .split(',')
                .map(|stub| String::from(stub.trim())),
        );
    }
}

fn get_comment_ranges(source_str: &str) -> Vec<std::ops::Range<usize>> {
    let mut in_line_comment = false;
    let mut in_multi_line_comment = false;
    let mut prev_byte: u8 = 0;
    let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();
    let mut range_start: usize = 0;
    let mut unicode_mb_width = 0;
    for (i, source_byte) in source_str.bytes().enumerate() {
        if unicode_mb_width > 1 {
            unicode_mb_width -= 1;
            continue;
        }
        // unicode
        if source_byte >= 128 {
            // this is a multibyte string.
            if source_byte > 0xF0 {
                unicode_mb_width = 3;
            } else if source_byte > 0xE0 {
                unicode_mb_width = 2;
            } else {
                unicode_mb_width = 1;
            }
            continue;
        }

        if in_line_comment {
            if source_byte == *NEWLINE_BYTE {
                in_line_comment = false;
                ranges.push(std::ops::Range {
                    start: range_start,
                    end: i,
                })
            }
        } else if in_multi_line_comment {
            if source_byte == *SLASH_BYTE && prev_byte == *STAR_BYTE {
                in_multi_line_comment = false;
                ranges.push(std::ops::Range {
                    start: range_start,
                    end: i,
                })
            }
        } else if source_byte == *SLASH_BYTE && prev_byte == *SLASH_BYTE {
            in_line_comment = true;
            range_start = i - 1;
        } else if source_byte == *STAR_BYTE && prev_byte == *SLASH_BYTE {
            in_multi_line_comment = true;
            range_start = i - 1;
        }

        prev_byte = source_byte
    }

    return ranges;
}

pub fn parse_source_text_imports(file_text: &str) -> SourceFileImportData {
    let mut import_paths_map = HashMap::<String, Option<HashSet<String>>>::new();

    let comment_ranges = get_comment_ranges(file_text);

    for captures in IMPORT_REGEX.captures_iter(&file_text) {
        let collected = captures
            .iter()
            .filter(|x| x.is_some())
            .map(|x| match x {
                Some(x) => x,
                _ => panic!("had Nothing after filtering out Nothing"),
            })
            .collect::<Vec<regex::Match>>();

        if collected.len() != 0 {
            let match_start_pos = collected.get(0).unwrap().start();
            if comment_ranges
                .iter()
                .any(|a| a.start <= match_start_pos && a.end >= match_start_pos)
            {
                // if this was captured, ignore it.
                continue;
            }
        }

        if collected.len() == 2 {
            // did not capture any names, this is just the import path
            let import_path_match: Option<&regex::Match> = collected.get(1);
            if import_path_match.is_some() {
                let import_path = String::from(import_path_match.unwrap().as_str());
                import_paths_map.insert(import_path, Option::None);
            }
        } else if collected.len() == 3 {
            // captured names and import paths
            let name_match: Option<&regex::Match> = collected.get(1);
            let import_path_match: Option<&regex::Match> = collected.get(2);
            if import_path_match.is_some() && name_match.is_some() {
                let import_path = import_path_match.unwrap().as_str();
                let name_stub = name_match.unwrap().as_str();
                let names: HashSet<String> = parse_imported_names(name_stub);
                import_paths_map.insert(String::from(import_path), Option::Some(names));
            }
        } else {
            panic!(format!("collected len was neither 1 nor 2 {:?}", collected));
        }
    }

    return SourceFileImportData {
        imports: import_paths_map,
    };
}

pub fn parse_source_file_imports(source_file_path: &Path) -> SourceFileImportData {
    let file_text: String = std::fs::read_to_string(source_file_path).expect(&format!(
        "error opening source file \"{:?}\"",
        source_file_path
    ));

    parse_source_text_imports(&file_text)
}

lazy_static! {
    static ref IMPORT_REGEX: regex::Regex = Regex::new(
        r###"(?:import|export) (?:\s*(\*)\s*|([\w]+)|(\{(?m:\s*(?:(?:.*(?:,\s*)?)+)\s*)\})) from (?:'(.*)'|"(.*)")|require\((?m:\s*(?:"([^)]*)"|'([^)]*)')\s*)\)|import\((?m:\s*"([^)]*)"|'([^)]*)'\s*)\)"###,
    )
    .unwrap();
}

lazy_static! {
    static ref LINE_COMMENT_REGEX: regex::Regex = Regex::new(r###"^\s*//"###,).unwrap();
}

lazy_static! {
    static ref NEWLINE_BYTE: u8 = {
        let newline_bytes = "\n".as_bytes();
        if newline_bytes.len() != 1 {
            panic!(
                "'\\n' is supposed to be a single byte character, but got {:?}",
                newline_bytes
            );
        }

        newline_bytes[0]
    };
}

lazy_static! {
    static ref SLASH_BYTE: u8 = {
        let newline_bytes = "/".as_bytes();
        if newline_bytes.len() != 1 {
            panic!(
                "'/' is supposed to be a single byte character, but got {:?}",
                newline_bytes
            );
        }

        newline_bytes[0]
    };
}

lazy_static! {
    static ref STAR_BYTE: u8 = {
        let newline_bytes = "*".as_bytes();
        if newline_bytes.len() != 1 {
            panic!(
                "'*' is supposed to be a single byte character, but got {:?}",
                newline_bytes
            );
        }

        newline_bytes[0]
    };
}

#[cfg(test)]
mod tests {
    use crate::parse_source_text_imports;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    macro_rules! map(
        { $($key:expr => $value:expr),+ } => {
            {
                let mut m = ::std::collections::HashMap::new();
                $(
                    m.insert(String::from($key), $value);
                )+
                m
            }
         };
    );

    macro_rules! set(
        { $($member:expr),+ } => {
            {
                HashSet::from_iter(vec!(
                    $(
                        String::from($member),
                    )+
                ))
            }
         };
    );

    #[test]
    fn parse_only_import_text_dblquote() {
        let r = parse_source_text_imports(r#"import a from "some-path""#);

        assert_eq!(
            r.imports,
            map!(
                "some-path" => Option::Some(set!("default"))
            )
        )
    }

    #[test]
    fn parse_only_import_text_snglquote() {
        let r = parse_source_text_imports(r#"import a from 'some-path'"#);

        assert_eq!(
            r.imports,
            map!(
                "some-path" => Option::Some(set!("default"))
            )
        )
    }

    #[test]
    fn parse_single_linebreak_import_statement() {
        let r = parse_source_text_imports(
            r#"
          import {
              singleImportWithLineBreak
          } from "some-multi-line-import-path"
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "some-multi-line-import-path" => Option::Some(set!("singleImportWithLineBreak"))
            )
        )
    }

    #[test]
    fn parse_import_statements() {
        let r = parse_source_text_imports(
            r#"import { a, b, c } from "some-path"
          import someDefault from "some-other-path"
          import {
              multi,
              line,
              importedNames
          } from "some-multi-line-import-path"
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "some-multi-line-import-path" => Option::Some(set!("multi", "line", "importedNames")),
                "some-other-path" => Option::Some(set!("default")),
                "some-path" => Option::Some(set!("a", "b", "c"))
            )
        )
    }

    #[test]
    fn parse_require_sngl_quote() {
        let r = parse_source_text_imports(r#"require('some-require-path')"#);

        assert_eq!(
            r.imports,
            map!(
                "some-require-path" => Option::None
            )
        )
    }

    #[test]
    fn parse_require_dbl_quote() {
        let r = parse_source_text_imports(r#"require("some-require-path")"#);

        assert_eq!(
            r.imports,
            map!(
                "some-require-path" => Option::None
            )
        )
    }

    #[test]
    fn parse_import_fn_sngl_quote() {
        let r = parse_source_text_imports(r#"import('some-import_fn-path')"#);

        assert_eq!(
            r.imports,
            map!(
                "some-import_fn-path" => Option::None
            )
        )
    }

    #[test]
    fn parse_import_fn_dbl_quote() {
        let r = parse_source_text_imports(r#"import("some-import_fn-path")"#);

        assert_eq!(
            r.imports,
            map!(
                "some-import_fn-path" => Option::None
            )
        )
    }

    #[test]
    fn parse_multiple_import_one_line() {
        let r = parse_source_text_imports(
            r#"
            return someCondition() ? import('./importOne') : import('./importOther');
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "./importOne" => Option::None,
                "./importOther" => Option::None
            )
        );
    }

    #[test]
    fn parse_multiple_require_one_line() {
        let r = parse_source_text_imports(
            r#"
            return someCondition() ? require('./importOne') : require('./importOther');
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "./importOne" => Option::None,
                "./importOther" => Option::None
            )
        );
    }

    #[test]
    fn parse_export_star_statement() {
        let r = parse_source_text_imports(
            r#"
            export * from './blahblahblah';
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "./blahblahblah" => Option::Some(set!("*"))
            )
        );
    }

    #[test]
    fn parse_require_in_line_comment() {
        let r = parse_source_text_imports(
            r#"
            require('test');
            require('toast'); // trailing line comments
            // require('bread');
            // don't use this either require('butter');
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "test" => Option::None,
                "toast" => Option::None
            )
        );
    }

    #[test]
    fn parse_require_in_comment_unicode_garbage() {
        // this test has a comment with a lot of unicode in it
        // in an attempt to desync the regex buffer index from the
        // character range index, to make sure we are using
        // both when indexing.
        let r = parse_source_text_imports(
            r#"/*
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
            require('lol')
            */
        "#,
        );

        assert_eq!(r.imports, std::collections::HashMap::new());
    }

    #[test]
    fn parse_require_in_multi_line_comment() {
        let r = parse_source_text_imports(
            r#"
            require('test');
            /**
             * require('bread') in docstring // trailing line comments
             */
            require('toast'); 
            /* don't use this either require('butter') */
            /*
            
            
            
            don't use this also
            
            
            require('tomato')
            
            */
        "#,
        );

        assert_eq!(
            r.imports,
            map!(
                "test" => Option::None,
                "toast" => Option::None
            )
        );
    }
}
