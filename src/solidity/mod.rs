pub use parser::{ast_from_json_file, traverse_nodes};
use std::collections::HashMap;
use std::path::Path;

pub mod parser;

pub fn process(path: &Path) -> HashMap<String, parser::AST> {
    parser::process(path)
}
