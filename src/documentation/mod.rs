pub use formatter::{FormatContext, node_to_html};
pub use parser::{DocumentationAST, DocumentationNode};

pub mod analyzer;
pub mod formatter;
pub mod parser;

// Re-export analyzer function
pub use analyzer::analyze;
