use std::{path::Path, vec};

use crate::solidity::{
  ASTNode, node_to_source_text,
  parser::{LiteralKind, SourceLocation, TypeDescriptions},
};

mod solidity;

fn main() {
  let html = node_to_source_text(&ASTNode::Assignment {
    node_id: 1532,
    src_location: SourceLocation {
      start: None,
      length: None,
      index: None,
    },
    operator: solidity::parser::AssignmentOperator::Assign,
    left_hand_side: Box::new(ASTNode::IdentifierPath {
      node_id: 1533,
      src_location: SourceLocation {
        start: None,
        length: None,
        index: None,
      },
      name: "my_var".to_string(),
      name_locations: vec![],
      referenced_declaration: 5320,
    }),
    right_and_side: Box::new(ASTNode::Literal {
      node_id: 420,
      src_location: SourceLocation {
        start: None,
        length: None,
        index: None,
      },
      hex_value: "0x2402".to_owned(),
      kind: LiteralKind::Number,
      type_descriptions: TypeDescriptions {
        type_identifier: "uint256".to_string(),
        type_string: "uint256".to_string(),
      },
      value: Option::Some("2022".to_owned()),
    }),
  });

  println!("{}", html);

  let project_path = Path::new("/home/john/olla/server/priv/audits/lido-crmv2");

  match solidity::analyze(project_path) {
    Ok(data_context) => {
      println!("Analyzer Results:");
      println!("In-scope files: {}", data_context.in_scope_files.len());
      for file in &data_context.in_scope_files {
        println!("  - {}", file);
      }
    }
    Err(e) => {
      eprintln!("Error analyzing project: {}", e);
      std::process::exit(1);
    }
  }
}
