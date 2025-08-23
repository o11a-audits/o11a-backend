pub use parser::{ast_from_json_file, traverse_nodes};
use std::collections::HashMap;
use std::path::Path;

pub mod parser;

pub fn process(path: &Path) -> Result<HashMap<String, Vec<parser::AST>>, String> {
    parser::process(path)
}
