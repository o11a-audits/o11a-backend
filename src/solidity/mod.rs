use std::collections::HashMap;
use std::path::Path;

pub mod parser;

pub fn process(path: &Path) -> HashMap<i32, parser::AST> {
    parser::process(path)
}
