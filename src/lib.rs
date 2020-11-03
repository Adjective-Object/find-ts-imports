extern crate lazy_static;
extern crate regex;

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::path::Path;

#[cfg(feature = "serde")]
#[macro_use]
extern crate serde;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SourceFileImportData {
    /**
     * Map from import path to the name that was imported
     */
    imports: HashMap<String, Option<HashSet<String>>>,
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

pub fn parse_source_text_imports(file_text: &str) -> SourceFileImportData {
    let mut import_paths_map = HashMap::<String, Option<HashSet<String>>>::new();

    for captures in IMPORT_REGEX.captures_iter(&file_text) {
        let collected = captures
            .iter()
            .filter(|x| x.is_some())
            .map(|x| match x {
                Some(x) => x,
                _ => panic!("had Nothing after filtering out Nothing"),
            })
            .collect::<Vec<regex::Match>>();

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
        r###"import (?:\s*(\*)\s*|([\w]+)|(\{(?m:\s*(?:(?:.*(?:,\s*)?)+)\s*)\})) from (?:'(.*)'|"(.*)")|require\((?m:\s*(?:"(.*)"|'(.*)')\s*)\)|import\((?m:\s*"(.*)"|'(.*)'\s*)\)"###,
    )
    .unwrap();
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
}
