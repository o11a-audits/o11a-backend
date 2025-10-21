// Run with `cargo run --bin formatter_test`
use o11a_backend::solidity::node_to_source_text;
use o11a_backend::solidity::parser::{
  ASTNode, AssignmentOperator, BinaryOperator, LiteralKind, SourceLocation,
  TypeDescriptions,
};

fn main() {
  // Example 1: Simple identifier
  println!("=== Example 1: Simple Identifier ===");
  let identifier = create_identifier("myVariable", 1);
  let formatted = node_to_source_text(&identifier);
  println!("{}\n", formatted);

  // Example 2: Binary operation (a + b)
  println!("=== Example 2: Binary Operation (a + b) ===");
  let binary_op = create_binary_operation();
  let formatted = node_to_source_text(&binary_op);
  println!("{}\n", formatted);

  // Example 3: Assignment (x = 5)
  println!("=== Example 3: Assignment (x = 5) ===");
  let assignment = create_assignment();
  let formatted = node_to_source_text(&assignment);
  println!("{}\n", formatted);

  // Example 4: Complex nested expression (a + b) * c
  println!("=== Example 4: Complex Expression ((a + b) * c) ===");
  let complex_expr = create_complex_expression();
  let formatted = node_to_source_text(&complex_expr);
  println!("{}\n", formatted);

  // Example 5: Block with multiple statements
  println!("=== Example 5: Block with Statements ===");
  let block = create_block_with_statements();
  let formatted = node_to_source_text(&block);
  println!("{}\n", formatted);
}

// Helper function to create a source location
fn create_source_location(
  start: usize,
  length: usize,
  index: usize,
) -> SourceLocation {
  SourceLocation {
    start: Some(start),
    length: Some(length),
    index: Some(index),
  }
}

// Helper function to create type descriptions
fn create_type_descriptions(type_string: &str) -> TypeDescriptions {
  TypeDescriptions {
    type_identifier: type_string.to_string(),
    type_string: type_string.to_string(),
  }
}

// Create a simple identifier node
fn create_identifier(name: &str, node_id: i32) -> ASTNode {
  ASTNode::Identifier {
    node_id,
    src_location: create_source_location(0, name.len(), 0),
    name: name.to_string(),
    overloaded_declarations: vec![],
    referenced_declaration: 0,
    type_descriptions: create_type_descriptions("uint256"),
  }
}

// Create a number literal node
fn create_literal(value: &str, node_id: i32) -> ASTNode {
  ASTNode::Literal {
    node_id,
    src_location: create_source_location(0, value.len(), 0),
    hex_value: format!("0x{}", value),
    kind: LiteralKind::Number,
    type_descriptions: create_type_descriptions("int_const"),
    value: Some(value.to_string()),
  }
}

// Create a binary operation: a + b
fn create_binary_operation() -> ASTNode {
  let left = create_identifier("a", 2);
  let right = create_identifier("b", 3);

  ASTNode::BinaryOperation {
    node_id: 1,
    src_location: create_source_location(0, 5, 0),
    left_expression: Box::new(left),
    operator: BinaryOperator::Add,
    right_expression: Box::new(right),
    type_descriptions: create_type_descriptions("uint256"),
  }
}

// Create an assignment: x = 5
fn create_assignment() -> ASTNode {
  let left = create_identifier("x", 5);
  let right = create_literal("5", 6);

  ASTNode::Assignment {
    node_id: 4,
    src_location: create_source_location(0, 5, 0),
    left_hand_side: Box::new(left),
    operator: AssignmentOperator::Assign,
    right_and_side: Box::new(right),
  }
}

// Create a complex expression: (a + b) * c
fn create_complex_expression() -> ASTNode {
  // Inner: a + b
  let a = create_identifier("a", 8);
  let b = create_identifier("b", 9);
  let inner_sum = ASTNode::BinaryOperation {
    node_id: 7,
    src_location: create_source_location(0, 5, 0),
    left_expression: Box::new(a),
    operator: BinaryOperator::Add,
    right_expression: Box::new(b),
    type_descriptions: create_type_descriptions("uint256"),
  };

  // Outer: (a + b) * c
  let c = create_identifier("c", 11);
  ASTNode::BinaryOperation {
    node_id: 10,
    src_location: create_source_location(0, 11, 0),
    left_expression: Box::new(inner_sum),
    operator: BinaryOperator::Multiply,
    right_expression: Box::new(c),
    type_descriptions: create_type_descriptions("uint256"),
  }
}

// Create a block with multiple statements
fn create_block_with_statements() -> ASTNode {
  let stmt1 = ASTNode::ExpressionStatement {
    node_id: 13,
    src_location: create_source_location(0, 5, 0),
    expression: Box::new(create_assignment()),
  };

  let stmt2 = ASTNode::ExpressionStatement {
    node_id: 14,
    src_location: create_source_location(6, 5, 0),
    expression: Box::new(create_binary_operation()),
  };

  ASTNode::Block {
    node_id: 12,
    src_location: create_source_location(0, 20, 0),
    statements: vec![stmt1, stmt2],
  }
}
