use crate::core;
use crate::core::topic;
use crate::core::{ContractKind, FunctionKind, ProjectPath};
use serde_json;
use std::collections::BTreeMap;
use std::path::Path;
use std::str::FromStr;
use std::{panic, vec};

pub fn process(
  project_root: &Path,
) -> Result<
  std::collections::BTreeMap<core::ProjectPath, Vec<SolidityAST>>,
  String,
> {
  let mut ast_map = std::collections::BTreeMap::new();

  // Look for the "out" directory in the project root
  let out_dir = project_root.join("out");
  if !out_dir.exists() || !out_dir.is_dir() {
    return Err(format!("'out' directory not found at {:?}", out_dir));
  }

  println!("Processing JSON files in directory: {:?}", out_dir);

  // Recursively traverse the out directory to find all JSON files
  traverse_directory(&out_dir, &project_root, &mut ast_map)?;

  let total_asts: usize = ast_map.values().map(|v| v.len()).sum();
  println!(
    "Successfully processed {} unique paths with {} total AST files",
    ast_map.len(),
    total_asts
  );

  Ok(ast_map)
}

fn traverse_directory(
  dir: &Path,
  project_root: &Path,
  ast_map: &mut std::collections::BTreeMap<core::ProjectPath, Vec<SolidityAST>>,
) -> Result<(), String> {
  let entries = std::fs::read_dir(dir)
    .map_err(|e| format!("Failed to read directory {:?}: {}", dir, e))?;

  for entry in entries {
    let entry =
      entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
    let path = entry.path();

    if path.is_dir() {
      // Skip the build-info directory
      if let Some(dir_name) = path.file_name() {
        if dir_name == "build-info" {
          continue;
        }
      }
      // Recursively traverse subdirectories
      traverse_directory(&path, &project_root, ast_map)?;
    } else if path.is_file() {
      if let Some(extension) = path.extension() {
        if extension == "json" {
          println!("Processing JSON file: {:?}", path);
          let ast = ast_from_json_file(&path.to_string_lossy(), &project_root)
            .map_err(|e| {
              format!("Failed to parse JSON file {:?}: {}", path, e)
            })?;

          ast_map
            .entry(ast.project_path.clone())
            .or_insert_with(Vec::new)
            .push(ast);
        }
      }
    }
  }

  Ok(())
}

pub fn ast_from_json_file(
  file_path: &str,
  project_root: &Path,
) -> Result<SolidityAST, String> {
  let json = std::fs::read_to_string(file_path)
    .map_err(|e| format!("Failed to read file: {}", e))?;

  // Parse the JSON string
  let parsed: serde_json::Value = serde_json::from_str(&json)
    .map_err(|e| format!("Failed to parse JSON: {}", e))?;

  // Extract the "ast" object from the root
  let ast_obj = parsed
    .get("ast")
    .ok_or_else(|| "Missing 'ast' field in JSON".to_string())?;

  // Parse the required fields from the ast object
  let node_id = ast_obj
    .get("id")
    .and_then(|v| v.as_i64())
    .map(|v| v as i32)
    .ok_or_else(|| "Missing or invalid 'id' field in ast object".to_string())?;

  let absolute_path = ast_obj
    .get("absolutePath")
    .and_then(|v| v.as_str())
    .map(|s| s.to_string())
    .ok_or_else(|| {
      "Missing or invalid 'absolutePath' field in ast object".to_string()
    })?;
  let project_path = core::new_project_path(&absolute_path, project_root);

  // Read the original source file content
  let source_content = read_source_file(&project_path, &project_root)?;

  let nodes_array = ast_obj
    .get("nodes")
    .and_then(|v| v.as_array())
    .ok_or_else(|| {
      "Missing or invalid 'nodes' field in ast object".to_string()
    })?;

  // Parse each node in the nodes array
  let nodes: Result<Vec<ASTNode>, String> = nodes_array
    .iter()
    .map(|node_val| node_from_json(node_val, &source_content))
    .collect();

  let nodes = nodes?;

  Ok(SolidityAST {
    node_id,
    nodes,
    project_path,
    source_content,
  })
}

pub struct SolidityAST {
  pub node_id: i32,
  pub nodes: Vec<ASTNode>,
  pub project_path: ProjectPath,
  pub source_content: String,
}

impl SolidityAST {
  /// Get children nodes, resolving nodes that are stubs to their real nodes
  /// from the nodes map
  pub fn resolve_nodes(
    &self,
    nodes_map: &BTreeMap<topic::Topic, core::Node>,
  ) -> Vec<ASTNode> {
    self
      .nodes
      .iter()
      .map(|node| match node {
        ASTNode::Stub { topic, .. } => {
          if let Some(core::Node::Solidity(ast_node)) = nodes_map.get(topic) {
            ast_node.clone()
          } else {
            node.clone()
          }
        }
        _ => node.clone(),
      })
      .collect()
  }
}

fn read_source_file(
  project_path: &ProjectPath,
  project_root: &Path,
) -> Result<String, String> {
  // Create the absolute path to the source file
  let absolute_source_file_path =
    core::project_path_to_absolute_path(project_path, project_root);

  // Read the source file
  std::fs::read_to_string(&absolute_source_file_path).map_err(|e| {
    format!(
      "Failed to read source file {:?}: {}",
      absolute_source_file_path, e
    )
  })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SourceLocation {
  pub start: Option<usize>,
  pub length: Option<usize>,
  pub index: Option<usize>,
}

impl FromStr for SourceLocation {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let invalid_location = move || format!("{s} invalid source location");

    let mut split = s.split(':');
    let start = split
      .next()
      .ok_or_else(invalid_location)?
      .parse::<isize>()
      .map_err(|_| invalid_location())?;
    let length = split
      .next()
      .ok_or_else(invalid_location)?
      .parse::<isize>()
      .map_err(|_| invalid_location())?;
    let index = split
      .next()
      .ok_or_else(invalid_location)?
      .parse::<isize>()
      .map_err(|_| invalid_location())?;

    let start = if start < 0 {
      None
    } else {
      Some(start as usize)
    };
    let length = if length < 0 {
      None
    } else {
      Some(length as usize)
    };
    let index = if index < 0 {
      None
    } else {
      Some(index as usize)
    };

    Ok(Self {
      start,
      length,
      index,
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionStateMutability {
  Pure,
  View,
  NonPayable,
  Payable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionVisibility {
  Public,
  Private,
  Internal,
  External,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableVisibility {
  Public,
  Private,
  Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableMutability {
  Mutable,
  Immutable,
  Constant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageLocation {
  Default,
  Storage,
  Memory,
  Calldata,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum LiteralKind {
  Number,
  Bool,
  String,
  HexString,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeDescriptions {
  pub type_identifier: String,
  pub type_string: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionCallKind {
  FunctionCall,
  TypeConversion,
  StructConstructor,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
  Increment,  // ++
  Decrement,  // --
  Plus,       // +
  Minus,      // -
  BitwiseNot, // ~
  Not,        // !
  Delete,     // delete
}

impl FromStr for UnaryOperator {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "!" => Ok(UnaryOperator::Not),
      "++" => Ok(UnaryOperator::Increment),
      "--" => Ok(UnaryOperator::Decrement),
      "+" => Ok(UnaryOperator::Plus),
      "-" => Ok(UnaryOperator::Minus),
      "~" => Ok(UnaryOperator::BitwiseNot),
      "delete" => Ok(UnaryOperator::Delete),
      _ => Err(format!("Invalid unary operator: {}", s)),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
  // Arithmetic
  Add,      // +
  Subtract, // -
  Multiply, // *
  Divide,   // /
  Modulo,   // %
  Power,    // **

  // Comparison
  Equal,              // ==
  NotEqual,           // !=
  LessThan,           // <
  LessThanOrEqual,    // <=
  GreaterThan,        // >
  GreaterThanOrEqual, // >=

  // Logical
  And, // &&
  Or,  // ||

  // Bitwise
  BitwiseAnd, // &
  BitwiseOr,  // |
  BitwiseXor, // ^
  LeftShift,  // <<
  RightShift, // >>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignmentOperator {
  Assign,           // =
  AddAssign,        // +=
  SubtractAssign,   // -=
  MultiplyAssign,   // *=
  DivideAssign,     // /=
  ModuloAssign,     // %=
  BitwiseAndAssign, // &=
  BitwiseOrAssign,  // |=
  BitwiseXorAssign, // ^=
  LeftShiftAssign,  // <<=
  RightShiftAssign, // >>=
}

impl FromStr for FunctionKind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "constructor" => Ok(FunctionKind::Constructor),
      "function" => Ok(FunctionKind::Function),
      "fallback" => Ok(FunctionKind::Fallback),
      "receive" => Ok(FunctionKind::Receive),
      "freeFunction" => Ok(FunctionKind::FreeFunction),
      _ => Err(format!("Unknown function kind: {}", s)),
    }
  }
}

impl FromStr for FunctionStateMutability {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "pure" => Ok(FunctionStateMutability::Pure),
      "view" => Ok(FunctionStateMutability::View),
      "nonpayable" => Ok(FunctionStateMutability::NonPayable),
      "payable" => Ok(FunctionStateMutability::Payable),
      _ => Err(format!("Unknown state mutability: {}", s)),
    }
  }
}

impl FromStr for FunctionVisibility {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "public" => Ok(FunctionVisibility::Public),
      "private" => Ok(FunctionVisibility::Private),
      "internal" => Ok(FunctionVisibility::Internal),
      "external" => Ok(FunctionVisibility::External),
      _ => Err(format!("Unknown visibility: {}", s)),
    }
  }
}

impl FromStr for VariableVisibility {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "public" => Ok(VariableVisibility::Public),
      "private" => Ok(VariableVisibility::Private),
      "internal" => Ok(VariableVisibility::Internal),
      _ => Err(format!("Unknown variable visibility: {}", s)),
    }
  }
}

impl FromStr for LiteralKind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "number" => Ok(LiteralKind::Number),
      "bool" => Ok(LiteralKind::Bool),
      "string" => Ok(LiteralKind::String),
      "hexString" => Ok(LiteralKind::HexString),
      _ => Err(format!("Unknown literal kind: {}", s)),
    }
  }
}

impl FromStr for FunctionCallKind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "functionCall" => Ok(FunctionCallKind::FunctionCall),
      "typeConversion" => Ok(FunctionCallKind::TypeConversion),
      "structConstructorCall" => Ok(FunctionCallKind::StructConstructor),
      _ => Err(format!("Unknown function call kind: {}", s)),
    }
  }
}

impl FromStr for BinaryOperator {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "+" => Ok(BinaryOperator::Add),
      "-" => Ok(BinaryOperator::Subtract),
      "*" => Ok(BinaryOperator::Multiply),
      "/" => Ok(BinaryOperator::Divide),
      "%" => Ok(BinaryOperator::Modulo),
      "**" => Ok(BinaryOperator::Power),
      "==" => Ok(BinaryOperator::Equal),
      "!=" => Ok(BinaryOperator::NotEqual),
      "<" => Ok(BinaryOperator::LessThan),
      "<=" => Ok(BinaryOperator::LessThanOrEqual),
      ">" => Ok(BinaryOperator::GreaterThan),
      ">=" => Ok(BinaryOperator::GreaterThanOrEqual),
      "&&" => Ok(BinaryOperator::And),
      "||" => Ok(BinaryOperator::Or),
      "&" => Ok(BinaryOperator::BitwiseAnd),
      "|" => Ok(BinaryOperator::BitwiseOr),
      "^" => Ok(BinaryOperator::BitwiseXor),
      "<<" => Ok(BinaryOperator::LeftShift),
      ">>" => Ok(BinaryOperator::RightShift),
      _ => Err(format!("Unknown binary operator: {}", s)),
    }
  }
}

impl FromStr for AssignmentOperator {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "=" => Ok(AssignmentOperator::Assign),
      "+=" => Ok(AssignmentOperator::AddAssign),
      "-=" => Ok(AssignmentOperator::SubtractAssign),
      "*=" => Ok(AssignmentOperator::MultiplyAssign),
      "/=" => Ok(AssignmentOperator::DivideAssign),
      "%=" => Ok(AssignmentOperator::ModuloAssign),
      "&=" => Ok(AssignmentOperator::BitwiseAndAssign),
      "|=" => Ok(AssignmentOperator::BitwiseOrAssign),
      "^=" => Ok(AssignmentOperator::BitwiseXorAssign),
      "<<=" => Ok(AssignmentOperator::LeftShiftAssign),
      ">>=" => Ok(AssignmentOperator::RightShiftAssign),
      _ => Err(format!("Unknown assignment operator: {}", s)),
    }
  }
}

impl FromStr for VariableMutability {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "mutable" => Ok(VariableMutability::Mutable),
      "immutable" => Ok(VariableMutability::Immutable),
      "constant" => Ok(VariableMutability::Constant),
      _ => Err(format!("Unknown mutability: {}", s)),
    }
  }
}

impl FromStr for StorageLocation {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "default" => Ok(StorageLocation::Default),
      "storage" => Ok(StorageLocation::Storage),
      "memory" => Ok(StorageLocation::Memory),
      "calldata" => Ok(StorageLocation::Calldata),
      _ => Err(format!("Invalid storage location: {}", s)),
    }
  }
}

impl FromStr for ContractKind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "contract" => Ok(ContractKind::Contract),
      "library" => Ok(ContractKind::Library),
      "abstract" => Ok(ContractKind::Abstract),
      "interface" => Ok(ContractKind::Interface),
      _ => Err(format!("Invalid contract kind: {}", s)),
    }
  }
}

impl TypeDescriptions {
  pub fn from_json(value: &serde_json::Value) -> Result<Self, String> {
    let type_identifier = value
      .get("typeIdentifier")
      .and_then(|v| v.as_str())
      .ok_or_else(|| {
        format!("TypeDescriptions missing typeIdentifier: {:?}", value)
      })?
      .to_string();
    let type_string = value
      .get("typeString")
      .and_then(|v| v.as_str())
      .ok_or_else(|| {
        format!("TypeDescriptions missing typeString: {:?}", value)
      })?
      .to_string();

    Ok(TypeDescriptions {
      type_identifier,
      type_string,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
  // Expression nodes
  Assignment {
    node_id: i32,
    src_location: SourceLocation,
    operator: AssignmentOperator,
    right_hand_side: Box<ASTNode>,
    left_hand_side: Box<ASTNode>,
  },
  BinaryOperation {
    node_id: i32,
    src_location: SourceLocation,
    left_expression: Box<ASTNode>,
    operator: BinaryOperator,
    right_expression: Box<ASTNode>,
    type_descriptions: TypeDescriptions,
  },
  Conditional {
    node_id: i32,
    src_location: SourceLocation,
    condition: Box<ASTNode>,
    true_expression: Box<ASTNode>,
    false_expression: Option<Box<ASTNode>>,
  },
  ElementaryTypeNameExpression {
    node_id: i32,
    src_location: SourceLocation,
    type_descriptions: TypeDescriptions,
    type_name: Box<ASTNode>,
  },
  FunctionCall {
    node_id: i32,
    src_location: SourceLocation,
    arguments: Vec<ASTNode>,
    expression: Box<ASTNode>,
    kind: FunctionCallKind,
    name_locations: Vec<SourceLocation>,
    names: Vec<String>,
    try_call: bool,
    type_descriptions: TypeDescriptions,
  },
  FunctionCallOptions {
    node_id: i32,
    src_location: SourceLocation,
    expression: Box<ASTNode>,
    options: Vec<ASTNode>,
  },
  Identifier {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    overloaded_declarations: Vec<i32>,
    referenced_declaration: i32,
    type_descriptions: TypeDescriptions,
  },
  IdentifierPath {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    name_locations: Vec<SourceLocation>,
    referenced_declaration: i32,
  },
  IndexAccess {
    node_id: i32,
    src_location: SourceLocation,
    base_expression: Box<ASTNode>,
    // index_expression is None in this case:
    // `RawTx1559 memory rawTx = abi.decode(parsedDeployData, (RawTx1559));`
    index_expression: Option<Box<ASTNode>>,
  },
  IndexRangeAccess {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
    body: Option<Box<ASTNode>>,
  },
  Literal {
    node_id: i32,
    src_location: SourceLocation,
    hex_value: String,
    kind: LiteralKind,
    type_descriptions: TypeDescriptions,
    value: Option<String>,
  },
  MemberAccess {
    node_id: i32,
    src_location: SourceLocation,
    expression: Box<ASTNode>,
    member_location: SourceLocation,
    member_name: String,
  },
  NewExpression {
    node_id: i32,
    src_location: SourceLocation,
    type_name: Box<ASTNode>,
  },
  TupleExpression {
    node_id: i32,
    src_location: SourceLocation,
    components: Vec<ASTNode>,
  },
  UnaryOperation {
    node_id: i32,
    src_location: SourceLocation,
    prefix: bool,
    operator: UnaryOperator,
    sub_expression: Box<ASTNode>,
  },
  EnumValue {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    name_location: SourceLocation,
  },

  // Statement nodes
  Block {
    node_id: i32,
    src_location: SourceLocation,
    statements: Vec<ASTNode>,
  },
  SemanticBlock {
    node_id: i32,
    src_location: SourceLocation,
    documentation: Option<String>,
    statements: Vec<ASTNode>,
  },
  Break {
    node_id: i32,
    src_location: SourceLocation,
  },
  Continue {
    node_id: i32,
    src_location: SourceLocation,
  },
  DoWhileStatement {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
    body: Option<Box<ASTNode>>,
  },
  EmitStatement {
    node_id: i32,
    src_location: SourceLocation,
    event_call: Box<ASTNode>,
  },
  ExpressionStatement {
    node_id: i32,
    src_location: SourceLocation,
    expression: Box<ASTNode>,
  },
  ForStatement {
    node_id: i32,
    src_location: SourceLocation,
    body: Box<ASTNode>,
    condition: Option<Box<ASTNode>>,
    initialization_expression: Option<Box<ASTNode>>,
    is_simple_counter_loop: bool,
    loop_expression: Option<Box<ASTNode>>,
  },
  IfStatement {
    node_id: i32,
    src_location: SourceLocation,
    condition: Box<ASTNode>,
    true_body: Box<ASTNode>,
    false_body: Option<Box<ASTNode>>,
  },
  InlineAssembly {
    node_id: i32,
    src_location: SourceLocation,
  },
  PlaceholderStatement {
    node_id: i32,
    src_location: SourceLocation,
  },
  Return {
    node_id: i32,
    src_location: SourceLocation,
    expression: Option<Box<ASTNode>>,
    function_return_parameters: i32,
  },
  RevertStatement {
    node_id: i32,
    src_location: SourceLocation,
    error_call: Box<ASTNode>,
  },
  TryStatement {
    node_id: i32,
    src_location: SourceLocation,
    clauses: Vec<ASTNode>,
    external_call: Box<ASTNode>,
  },
  UncheckedBlock {
    node_id: i32,
    src_location: SourceLocation,
    statements: Vec<ASTNode>,
  },
  VariableDeclarationStatement {
    node_id: i32,
    src_location: SourceLocation,
    declarations: Vec<ASTNode>,
    initial_value: Option<Box<ASTNode>>,
  },
  VariableDeclaration {
    node_id: i32,
    src_location: SourceLocation,
    constant: bool,
    function_selector: Option<String>,
    mutability: VariableMutability,
    name: String,
    name_location: SourceLocation,
    scope: i32,
    state_variable: bool,
    storage_location: StorageLocation,
    type_name: Box<ASTNode>,
    value: Option<Box<ASTNode>>,
    visibility: VariableVisibility,
  },
  WhileStatement {
    node_id: i32,
    src_location: SourceLocation,
    condition: Box<ASTNode>,
    body: Option<Box<ASTNode>>,
  },

  // Definition nodes
  ContractDefinition {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
    abstract_: bool,
    base_contracts: Vec<ASTNode>,
    name: String,
    name_location: SourceLocation,
    contract_kind: ContractKind,
  },
  FunctionDefinition {
    node_id: i32,
    src_location: SourceLocation,
    body: Option<Box<ASTNode>>,
    documentation: Option<Box<ASTNode>>,
    implemented: bool,
    kind: FunctionKind,
    modifiers: Vec<ASTNode>,
    name: String,
    name_location: SourceLocation,
    parameters: Box<ASTNode>,
    return_parameters: Box<ASTNode>,
    scope: i32,
    state_mutability: FunctionStateMutability,
    virtual_: bool,
    visibility: FunctionVisibility,
  },
  EventDefinition {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    name_location: SourceLocation,
    parameters: Box<ASTNode>,
  },
  ErrorDefinition {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    name_location: SourceLocation,
    parameters: Box<ASTNode>,
  },
  ModifierDefinition {
    node_id: i32,
    src_location: SourceLocation,
    body: Box<ASTNode>,
    documentation: Option<Box<ASTNode>>,
    name: String,
    name_location: SourceLocation,
    parameters: Box<ASTNode>,
    virtual_: bool,
    visibility: FunctionVisibility,
  },
  StructDefinition {
    node_id: i32,
    src_location: SourceLocation,
    members: Vec<ASTNode>,
    canonical_name: String,
    name: String,
    name_location: SourceLocation,
    visibility: VariableVisibility,
  },
  EnumDefinition {
    node_id: i32,
    src_location: SourceLocation,
    members: Vec<ASTNode>,
    canonical_name: String,
    name: String,
    name_location: SourceLocation,
  },
  UserDefinedValueTypeDefinition {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
    underlying_type: Box<ASTNode>,
  },

  // Directive nodes
  PragmaDirective {
    node_id: i32,
    src_location: SourceLocation,
    literals: Vec<String>,
  },
  ImportDirective {
    node_id: i32,
    src_location: SourceLocation,
    absolute_path: String,
    file: String,
    source_unit: i32,
  },
  UsingForDirective {
    node_id: i32,
    src_location: SourceLocation,
    global: bool,
    library_name: Option<Box<ASTNode>>,
    // This field exists but is complex, so I am commenting it out until we
    // want to add support for it later. An example of this is:
    // `using { unwrap, lt as <, gt as > } for Slot global;` with the node:
    //{
    //   "id": 81050,
    //   "nodeType": "UsingForDirective",
    //   "src": "458:51:133",
    //   "nodes": [],
    //   "functionList": [
    //     {
    //       "function": {
    //         "id": 81045,
    //         "name": "unwrap",
    //         "nameLocations": [
    //           "466:6:133"
    //         ],
    //         "nodeType": "IdentifierPath",
    //         "referencedDeclaration": 81004,
    //         "src": "466:6:133"
    //       }
    //     },
    //     {
    //       "definition": {
    //         "id": 81046,
    //         "name": "lt",
    //         "nameLocations": [
    //           "474:2:133"
    //         ],
    //         "nodeType": "IdentifierPath",
    //         "referencedDeclaration": 81044,
    //         "src": "474:2:133"
    //       },
    //       "operator": "<"
    //     },
    //     {
    //       "definition": {
    //         "id": 81047,
    //         "name": "gt",
    //         "nameLocations": [
    //           "483:2:133"
    //         ],
    //         "nodeType": "IdentifierPath",
    //         "referencedDeclaration": 81024,
    //         "src": "483:2:133"
    //       },
    //       "operator": ">"
    //     }
    //   ],
    //   "global": true,
    //   "typeName": {
    //     "id": 81049,
    //     "nodeType": "UserDefinedTypeName",
    //     "pathNode": {
    //       "id": 81048,
    //       "name": "Slot",
    //       "nameLocations": [
    //         "497:4:133"
    //       ],
    //       "nodeType": "IdentifierPath",
    //       "referencedDeclaration": 80990,
    //       "src": "497:4:133"
    //     },
    //     "referencedDeclaration": 80990,
    //     "src": "497:4:133",
    //     "typeDescriptions": {
    //       "typeIdentifier": "t_userDefinedValueType$_Slot_$80990",
    //       "typeString": "Slot"
    //     }
    //   }
    // },
    // function_list: Option<Vec<ASTNode>>,
    type_name: Option<Box<ASTNode>>,
  },

  // Other nodes
  SourceUnit {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
  },
  InheritanceSpecifier {
    node_id: i32,
    src_location: SourceLocation,
    base_name: Box<ASTNode>,
  },
  ElementaryTypeName {
    node_id: i32,
    src_location: SourceLocation,
    name: String,
  },
  FunctionTypeName {
    node_id: i32,
    src_location: SourceLocation,
    parameter_types: Box<ASTNode>,
    return_parameter_types: Box<ASTNode>,
    state_mutability: FunctionStateMutability,
    visibility: FunctionVisibility,
  },
  ParameterList {
    node_id: i32,
    src_location: SourceLocation,
    parameters: Vec<ASTNode>,
  },
  TryCatchClause {
    node_id: i32,
    src_location: SourceLocation,
    error_name: String,
    block: Box<ASTNode>,
    parameters: Option<Box<ASTNode>>,
  },
  ModifierInvocation {
    node_id: i32,
    src_location: SourceLocation,
    modifier_name: Box<ASTNode>,
    arguments: Option<Vec<ASTNode>>,
  },
  UserDefinedTypeName {
    node_id: i32,
    src_location: SourceLocation,
    path_node: Box<ASTNode>,
    referenced_declaration: i32,
  },
  ArrayTypeName {
    node_id: i32,
    src_location: SourceLocation,
    base_type: Box<ASTNode>,
  },
  Mapping {
    node_id: i32,
    src_location: SourceLocation,
    key_name: Option<String>,
    key_name_location: SourceLocation,
    key_type: Box<ASTNode>,
    value_name: Option<String>,
    value_name_location: SourceLocation,
    value_type: Box<ASTNode>,
  },

  StructuredDocumentation {
    node_id: i32,
    src_location: SourceLocation,
    text: String,
  },

  // Placeholder for another type of node. This node is just used for
  // optimizations, so that a node can hold a placeholder for its children
  // instead of all of its real children (which can be expensive)
  Stub {
    node_id: i32,
    src_location: SourceLocation,
    topic: topic::Topic,
  },

  // Catch-all for unknown node types
  Other {
    node_id: i32,
    src_location: SourceLocation,
    nodes: Vec<ASTNode>,
    body: Option<Box<ASTNode>>,
    node_type: String,
  },
}

impl ASTNode {
  pub fn node_id(&self) -> i32 {
    match self {
      ASTNode::Assignment { node_id, .. } => *node_id,
      ASTNode::BinaryOperation { node_id, .. } => *node_id,
      ASTNode::Conditional { node_id, .. } => *node_id,
      ASTNode::ElementaryTypeNameExpression { node_id, .. } => *node_id,
      ASTNode::FunctionCall { node_id, .. } => *node_id,
      ASTNode::FunctionCallOptions { node_id, .. } => *node_id,
      ASTNode::Identifier { node_id, .. } => *node_id,
      ASTNode::IdentifierPath { node_id, .. } => *node_id,
      ASTNode::IndexAccess { node_id, .. } => *node_id,
      ASTNode::IndexRangeAccess { node_id, .. } => *node_id,
      ASTNode::Literal { node_id, .. } => *node_id,
      ASTNode::MemberAccess { node_id, .. } => *node_id,
      ASTNode::NewExpression { node_id, .. } => *node_id,
      ASTNode::TupleExpression { node_id, .. } => *node_id,
      ASTNode::UnaryOperation { node_id, .. } => *node_id,
      ASTNode::EnumValue { node_id, .. } => *node_id,
      ASTNode::Block { node_id, .. } => *node_id,
      ASTNode::SemanticBlock { node_id, .. } => *node_id,
      ASTNode::Break { node_id, .. } => *node_id,
      ASTNode::Continue { node_id, .. } => *node_id,
      ASTNode::DoWhileStatement { node_id, .. } => *node_id,
      ASTNode::EmitStatement { node_id, .. } => *node_id,
      ASTNode::ExpressionStatement { node_id, .. } => *node_id,
      ASTNode::ForStatement { node_id, .. } => *node_id,
      ASTNode::IfStatement { node_id, .. } => *node_id,
      ASTNode::InlineAssembly { node_id, .. } => *node_id,
      ASTNode::PlaceholderStatement { node_id, .. } => *node_id,
      ASTNode::Return { node_id, .. } => *node_id,
      ASTNode::RevertStatement { node_id, .. } => *node_id,
      ASTNode::TryStatement { node_id, .. } => *node_id,
      ASTNode::UncheckedBlock { node_id, .. } => *node_id,
      ASTNode::VariableDeclarationStatement { node_id, .. } => *node_id,
      ASTNode::VariableDeclaration { node_id, .. } => *node_id,
      ASTNode::WhileStatement { node_id, .. } => *node_id,
      ASTNode::ContractDefinition { node_id, .. } => *node_id,
      ASTNode::FunctionDefinition { node_id, .. } => *node_id,
      ASTNode::EventDefinition { node_id, .. } => *node_id,
      ASTNode::ErrorDefinition { node_id, .. } => *node_id,
      ASTNode::ModifierDefinition { node_id, .. } => *node_id,
      ASTNode::StructDefinition { node_id, .. } => *node_id,
      ASTNode::EnumDefinition { node_id, .. } => *node_id,
      ASTNode::UserDefinedValueTypeDefinition { node_id, .. } => *node_id,
      ASTNode::PragmaDirective { node_id, .. } => *node_id,
      ASTNode::ImportDirective { node_id, .. } => *node_id,
      ASTNode::UsingForDirective { node_id, .. } => *node_id,
      ASTNode::SourceUnit { node_id, .. } => *node_id,
      ASTNode::InheritanceSpecifier { node_id, .. } => *node_id,
      ASTNode::ElementaryTypeName { node_id, .. } => *node_id,
      ASTNode::FunctionTypeName { node_id, .. } => *node_id,
      ASTNode::ParameterList { node_id, .. } => *node_id,
      ASTNode::TryCatchClause { node_id, .. } => *node_id,
      ASTNode::ModifierInvocation { node_id, .. } => *node_id,
      ASTNode::UserDefinedTypeName { node_id, .. } => *node_id,
      ASTNode::ArrayTypeName { node_id, .. } => *node_id,
      ASTNode::Mapping { node_id, .. } => *node_id,
      ASTNode::StructuredDocumentation { node_id, .. } => *node_id,
      ASTNode::Stub { node_id, .. } => *node_id,
      ASTNode::Other { node_id, .. } => *node_id,
    }
  }

  pub fn src_location(&self) -> &SourceLocation {
    match self {
      ASTNode::Assignment { src_location, .. } => src_location,
      ASTNode::BinaryOperation { src_location, .. } => src_location,
      ASTNode::Conditional { src_location, .. } => src_location,
      ASTNode::ElementaryTypeNameExpression { src_location, .. } => {
        src_location
      }
      ASTNode::FunctionCall { src_location, .. } => src_location,
      ASTNode::FunctionCallOptions { src_location, .. } => src_location,
      ASTNode::Identifier { src_location, .. } => src_location,
      ASTNode::IdentifierPath { src_location, .. } => src_location,
      ASTNode::IndexAccess { src_location, .. } => src_location,
      ASTNode::IndexRangeAccess { src_location, .. } => src_location,
      ASTNode::Literal { src_location, .. } => src_location,
      ASTNode::MemberAccess { src_location, .. } => src_location,
      ASTNode::NewExpression { src_location, .. } => src_location,
      ASTNode::TupleExpression { src_location, .. } => src_location,
      ASTNode::UnaryOperation { src_location, .. } => src_location,
      ASTNode::EnumValue { src_location, .. } => src_location,
      ASTNode::Block { src_location, .. } => src_location,
      ASTNode::SemanticBlock { src_location, .. } => src_location,
      ASTNode::Break { src_location, .. } => src_location,
      ASTNode::Continue { src_location, .. } => src_location,
      ASTNode::DoWhileStatement { src_location, .. } => src_location,
      ASTNode::EmitStatement { src_location, .. } => src_location,
      ASTNode::ExpressionStatement { src_location, .. } => src_location,
      ASTNode::ForStatement { src_location, .. } => src_location,
      ASTNode::IfStatement { src_location, .. } => src_location,
      ASTNode::InlineAssembly { src_location, .. } => src_location,
      ASTNode::PlaceholderStatement { src_location, .. } => src_location,
      ASTNode::Return { src_location, .. } => src_location,
      ASTNode::RevertStatement { src_location, .. } => src_location,
      ASTNode::TryStatement { src_location, .. } => src_location,
      ASTNode::UncheckedBlock { src_location, .. } => src_location,
      ASTNode::VariableDeclarationStatement { src_location, .. } => {
        src_location
      }
      ASTNode::VariableDeclaration { src_location, .. } => src_location,
      ASTNode::WhileStatement { src_location, .. } => src_location,
      ASTNode::ContractDefinition { src_location, .. } => src_location,
      ASTNode::FunctionDefinition { src_location, .. } => src_location,
      ASTNode::EventDefinition { src_location, .. } => src_location,
      ASTNode::ErrorDefinition { src_location, .. } => src_location,
      ASTNode::ModifierDefinition { src_location, .. } => src_location,
      ASTNode::StructDefinition { src_location, .. } => src_location,
      ASTNode::EnumDefinition { src_location, .. } => src_location,
      ASTNode::UserDefinedValueTypeDefinition { src_location, .. } => {
        src_location
      }
      ASTNode::PragmaDirective { src_location, .. } => src_location,
      ASTNode::ImportDirective { src_location, .. } => src_location,
      ASTNode::UsingForDirective { src_location, .. } => src_location,
      ASTNode::SourceUnit { src_location, .. } => src_location,
      ASTNode::InheritanceSpecifier { src_location, .. } => src_location,
      ASTNode::ElementaryTypeName { src_location, .. } => src_location,
      ASTNode::FunctionTypeName { src_location, .. } => src_location,
      ASTNode::ParameterList { src_location, .. } => src_location,
      ASTNode::TryCatchClause { src_location, .. } => src_location,
      ASTNode::ModifierInvocation { src_location, .. } => src_location,
      ASTNode::UserDefinedTypeName { src_location, .. } => src_location,
      ASTNode::ArrayTypeName { src_location, .. } => src_location,
      ASTNode::Mapping { src_location, .. } => src_location,
      ASTNode::StructuredDocumentation { src_location, .. } => src_location,
      ASTNode::Stub { src_location, .. } => src_location,
      ASTNode::Other { src_location, .. } => src_location,
    }
  }

  pub fn nodes(&self) -> Vec<&ASTNode> {
    match self {
      ASTNode::Assignment {
        right_hand_side,
        left_hand_side,
        ..
      } => vec![right_hand_side, left_hand_side],
      ASTNode::BinaryOperation {
        left_expression,
        right_expression,
        ..
      } => vec![left_expression, right_expression],
      ASTNode::Conditional {
        condition,
        true_expression,
        false_expression,
        ..
      } => match false_expression {
        Some(false_expr) => vec![condition, true_expression, false_expr],
        None => vec![condition, true_expression],
      },
      ASTNode::ElementaryTypeNameExpression { .. } => vec![],
      ASTNode::FunctionCall {
        arguments,
        expression,
        ..
      } => {
        let mut result = vec![&**expression];
        for item in arguments {
          result.push(item);
        }
        result
      }
      ASTNode::FunctionCallOptions {
        expression,
        options,
        ..
      } => {
        let mut result = vec![&**expression];
        for item in options {
          result.push(item);
        }
        result
      }
      ASTNode::Identifier { .. } => vec![],
      ASTNode::IdentifierPath { .. } => vec![],
      ASTNode::IndexAccess {
        base_expression,
        index_expression,
        ..
      } => {
        let mut result = vec![&**base_expression];
        if let Some(index_expression) = index_expression {
          result.push(&**index_expression);
        }
        result
      }
      ASTNode::IndexRangeAccess { .. } => {
        panic!("IndexRangeAccess not implemented")
      }
      ASTNode::Literal { .. } => vec![],
      ASTNode::MemberAccess { expression, .. } => vec![expression],
      ASTNode::NewExpression { type_name, .. } => vec![type_name],
      ASTNode::TupleExpression { components, .. } => {
        let mut result = vec![];
        for item in components {
          result.push(item);
        }
        result
      }
      ASTNode::UnaryOperation { sub_expression, .. } => vec![sub_expression],
      ASTNode::EnumValue { .. } => vec![],
      ASTNode::Block { statements, .. } => {
        let mut result = vec![];
        for item in statements {
          result.push(item);
        }
        result
      }
      ASTNode::SemanticBlock { statements, .. } => {
        let mut result = vec![];
        for item in statements {
          result.push(item);
        }
        result
      }
      ASTNode::Break { .. } => vec![],
      ASTNode::Continue { .. } => vec![],
      ASTNode::DoWhileStatement { .. } => {
        panic!("DoWhileStatement not implemented")
      }
      ASTNode::EmitStatement { event_call, .. } => vec![event_call],
      ASTNode::ExpressionStatement { expression, .. } => vec![expression],
      ASTNode::ForStatement {
        body,
        condition,
        initialization_expression,
        loop_expression,
        ..
      } => {
        let mut result = vec![&**body];
        if let Some(initialization_expression) = initialization_expression {
          result.push(initialization_expression);
        }
        if let Some(condition) = condition {
          result.push(condition);
        }
        if let Some(loop_expression) = loop_expression {
          result.push(loop_expression);
        }
        result
      }
      ASTNode::IfStatement {
        condition,
        true_body,
        false_body,
        ..
      } => match false_body {
        Some(false_body) => vec![condition, true_body, false_body],
        None => vec![condition, true_body],
      },
      ASTNode::InlineAssembly { .. } => vec![],
      ASTNode::PlaceholderStatement { .. } => vec![],
      ASTNode::Return { expression, .. } => match expression {
        Some(expr) => vec![expr],
        None => vec![],
      },
      ASTNode::RevertStatement { error_call, .. } => vec![error_call],
      ASTNode::TryStatement {
        clauses,
        external_call,
        ..
      } => {
        let mut result = vec![&**external_call];
        for clause in clauses {
          result.push(clause);
        }
        result
      }
      ASTNode::UncheckedBlock { statements, .. } => {
        let mut result = vec![];
        for item in statements {
          result.push(item);
        }
        result
      }
      ASTNode::VariableDeclarationStatement {
        declarations,
        initial_value,
        ..
      } => {
        let mut result = vec![];
        for item in declarations {
          result.push(item);
        }
        match initial_value {
          Some(value) => result.push(value),
          None => {}
        }
        result
      }
      ASTNode::VariableDeclaration {
        type_name, value, ..
      } => match value {
        Some(val) => vec![type_name, val],
        None => vec![type_name],
      },
      ASTNode::WhileStatement {
        condition, body, ..
      } => match body {
        Some(body) => vec![body, condition],
        None => vec![condition],
      },
      ASTNode::ContractDefinition {
        nodes,
        base_contracts,
        ..
      } => {
        let mut result = vec![];
        for item in nodes {
          result.push(item);
        }
        for item in base_contracts {
          result.push(item);
        }
        result
      }
      ASTNode::FunctionDefinition {
        body,
        documentation,
        modifiers,
        parameters,
        return_parameters,
        ..
      } => {
        let mut result = vec![];
        match body {
          Some(body_node) => result.push(&**body_node),
          None => {}
        }
        match documentation {
          Some(doc) => result.push(&**doc),
          None => {}
        }
        for item in modifiers {
          result.push(item);
        }
        result.push(&**parameters);
        result.push(&**return_parameters);
        result
      }
      ASTNode::EventDefinition { parameters, .. } => vec![parameters],
      ASTNode::ErrorDefinition { parameters, .. } => vec![parameters],
      ASTNode::ModifierDefinition {
        body,
        documentation,
        parameters,
        ..
      } => {
        let mut result = vec![&**body, &**parameters];
        match documentation {
          Some(doc) => result.push(doc),
          None => {}
        }
        result
      }
      ASTNode::StructDefinition { members, .. } => {
        let mut result = vec![];
        for item in members {
          result.push(item);
        }
        result
      }
      ASTNode::EnumDefinition { members, .. } => {
        let mut result = vec![];
        for item in members {
          result.push(item);
        }
        result
      }
      ASTNode::UserDefinedValueTypeDefinition {
        underlying_type, ..
      } => vec![underlying_type],
      ASTNode::PragmaDirective { .. } => vec![],
      ASTNode::ImportDirective { .. } => vec![],
      ASTNode::UsingForDirective {
        library_name,
        type_name,
        ..
      } => {
        let mut result = vec![];
        if let Some(type_name) = type_name {
          result.push(&**type_name);
        }
        if let Some(lib_name) = library_name {
          result.push(&**lib_name);
        }
        result
      }
      ASTNode::SourceUnit { nodes, .. } => {
        let mut result = vec![];
        for item in nodes {
          result.push(item);
        }
        result
      }
      ASTNode::InheritanceSpecifier { base_name, .. } => vec![base_name],
      ASTNode::ElementaryTypeName { .. } => vec![],
      ASTNode::FunctionTypeName {
        parameter_types,
        return_parameter_types,
        ..
      } => {
        let mut result = vec![];
        result.push(&**parameter_types);
        result.push(&**return_parameter_types);
        result
      }
      ASTNode::ParameterList { parameters, .. } => {
        let mut result = vec![];
        for item in parameters {
          result.push(item);
        }
        result
      }
      ASTNode::TryCatchClause {
        block, parameters, ..
      } => match parameters {
        Some(params) => vec![&**block, &**params],
        None => vec![&**block],
      },
      ASTNode::ModifierInvocation {
        modifier_name,
        arguments,
        ..
      } => {
        let mut result = vec![&**modifier_name];
        match arguments {
          Some(args) => {
            for item in args {
              result.push(item);
            }
          }
          None => {}
        }
        result
      }
      ASTNode::UserDefinedTypeName { path_node, .. } => vec![path_node],
      ASTNode::ArrayTypeName { base_type, .. } => vec![base_type],
      ASTNode::Mapping {
        key_type,
        value_type,
        ..
      } => vec![key_type, value_type],
      ASTNode::StructuredDocumentation { .. } => vec![],
      ASTNode::Stub { .. } => vec![],
      ASTNode::Other { nodes, body, .. } => {
        let mut result = vec![];
        for item in nodes {
          result.push(item);
        }
        match body {
          Some(body_node) => result.push(body_node),
          None => {}
        }
        result
      }
    }
  }

  /// Get children nodes, resolving nodes that are stubs to their real nodes
  /// from the nodes map
  pub fn resolve_nodes(
    &self,
    nodes_map: &BTreeMap<topic::Topic, core::Node>,
  ) -> Vec<ASTNode> {
    let nodes = self.nodes();

    nodes
      .iter()
      .map(|node| match node {
        ASTNode::Stub { topic, .. } => {
          if let Some(core::Node::Solidity(ast_node)) = nodes_map.get(topic) {
            ast_node.clone()
          } else {
            (*node).clone()
          }
        }
        _ => (*node).clone(),
      })
      .collect()
  }

  /// Resolve the current node if it is a node stub
  pub fn resolve<'a>(
    &'a self,
    nodes_map: &'a BTreeMap<topic::Topic, core::Node>,
  ) -> &'a ASTNode {
    match self {
      ASTNode::Stub { topic, .. } => {
        if let Some(core::Node::Solidity(ast_node)) = nodes_map.get(topic) {
          ast_node
        } else {
          self
        }
      }
      _ => self,
    }
  }
}

fn node_to_stub(node: &ASTNode) -> ASTNode {
  ASTNode::Stub {
    node_id: node.node_id(),
    src_location: node.src_location().clone(),
    topic: topic::new_node_topic(node.node_id()),
  }
}

pub fn children_to_stubs(node: ASTNode) -> ASTNode {
  match node {
    ASTNode::Assignment {
      node_id,
      src_location,
      operator,
      right_hand_side,
      left_hand_side,
    } => ASTNode::Assignment {
      node_id: node_id,
      src_location: src_location,
      operator: operator,
      right_hand_side: Box::new(node_to_stub(&right_hand_side)),
      left_hand_side: Box::new(node_to_stub(&left_hand_side)),
    },
    ASTNode::BinaryOperation {
      node_id,
      src_location,
      left_expression,
      operator,
      right_expression,
      type_descriptions,
    } => ASTNode::BinaryOperation {
      node_id: node_id,
      src_location: src_location,
      left_expression: Box::new(node_to_stub(&left_expression)),
      operator: operator,
      right_expression: Box::new(node_to_stub(&right_expression)),
      type_descriptions: type_descriptions,
    },
    ASTNode::Conditional {
      node_id,
      src_location,
      condition,
      true_expression,
      false_expression,
    } => ASTNode::Conditional {
      node_id: node_id,
      src_location: src_location,
      condition: Box::new(node_to_stub(&condition)),
      true_expression: Box::new(node_to_stub(&true_expression)),
      false_expression: match false_expression {
        Some(expr) => Some(Box::new(node_to_stub(&expr))),
        None => None,
      },
    },
    ASTNode::ElementaryTypeNameExpression {
      node_id,
      src_location,
      type_descriptions,
      type_name,
    } => ASTNode::ElementaryTypeNameExpression {
      node_id: node_id,
      src_location: src_location,
      type_descriptions: type_descriptions,
      type_name: Box::new(node_to_stub(&type_name)),
    },
    ASTNode::FunctionCall {
      node_id,
      src_location,
      arguments,
      expression,
      kind,
      name_locations,
      names,
      try_call,
      type_descriptions,
    } => ASTNode::FunctionCall {
      node_id: node_id,
      src_location: src_location,
      arguments: arguments.iter().map(|n| node_to_stub(n)).collect(),
      expression: Box::new(node_to_stub(&expression)),
      kind: kind,
      name_locations: name_locations,
      names: names,
      try_call: try_call,
      type_descriptions: type_descriptions,
    },
    ASTNode::FunctionCallOptions {
      node_id,
      src_location,
      expression,
      options,
    } => ASTNode::FunctionCallOptions {
      node_id: node_id,
      src_location: src_location,
      expression: Box::new(node_to_stub(&expression)),
      options: options.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::Identifier {
      node_id,
      src_location,
      name,
      overloaded_declarations,
      referenced_declaration,
      type_descriptions,
    } => ASTNode::Identifier {
      node_id: node_id,
      src_location: src_location,
      name: name,
      overloaded_declarations: overloaded_declarations,
      referenced_declaration: referenced_declaration,
      type_descriptions: type_descriptions,
    },
    ASTNode::IdentifierPath {
      node_id,
      src_location,
      name,
      name_locations,
      referenced_declaration,
    } => ASTNode::IdentifierPath {
      node_id: node_id,
      src_location: src_location,
      name: name,
      name_locations: name_locations,
      referenced_declaration: referenced_declaration,
    },
    ASTNode::IndexAccess {
      node_id,
      src_location,
      base_expression,
      index_expression,
    } => ASTNode::IndexAccess {
      node_id: node_id,
      src_location: src_location,
      base_expression: Box::new(node_to_stub(&base_expression)),
      index_expression: match index_expression {
        Some(expr) => Some(Box::new(node_to_stub(&expr))),
        None => None,
      },
    },
    ASTNode::IndexRangeAccess {
      node_id,
      src_location,
      nodes,
      body,
    } => ASTNode::IndexRangeAccess {
      node_id: node_id,
      src_location: src_location,
      nodes: nodes.iter().map(|n| node_to_stub(n)).collect(),
      body: match body {
        Some(b) => Some(Box::new(node_to_stub(&b))),
        None => None,
      },
    },
    ASTNode::Literal {
      node_id,
      src_location,
      hex_value,
      kind,
      type_descriptions,
      value,
    } => ASTNode::Literal {
      node_id: node_id,
      src_location: src_location,
      hex_value: hex_value,
      kind: kind,
      type_descriptions: type_descriptions,
      value: value,
    },
    ASTNode::MemberAccess {
      node_id,
      src_location,
      expression,
      member_location,
      member_name,
    } => ASTNode::MemberAccess {
      node_id: node_id,
      src_location: src_location,
      expression: Box::new(node_to_stub(&expression)),
      member_location: member_location,
      member_name: member_name,
    },
    ASTNode::NewExpression {
      node_id,
      src_location,
      type_name,
    } => ASTNode::NewExpression {
      node_id: node_id,
      src_location: src_location,
      type_name: Box::new(node_to_stub(&type_name)),
    },
    ASTNode::TupleExpression {
      node_id,
      src_location,
      components,
    } => ASTNode::TupleExpression {
      node_id: node_id,
      src_location: src_location,
      components: components.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::UnaryOperation {
      node_id,
      src_location,
      prefix,
      operator,
      sub_expression,
    } => ASTNode::UnaryOperation {
      node_id: node_id,
      src_location: src_location,
      prefix: prefix,
      operator: operator,
      sub_expression: Box::new(node_to_stub(&sub_expression)),
    },
    ASTNode::EnumValue {
      node_id,
      src_location,
      name,
      name_location,
    } => ASTNode::EnumValue {
      node_id: node_id,
      src_location: src_location,
      name: name,
      name_location: name_location,
    },
    ASTNode::Block {
      node_id,
      src_location,
      statements,
    } => ASTNode::Block {
      node_id: node_id,
      src_location: src_location,
      statements: statements.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::SemanticBlock {
      node_id,
      src_location,
      documentation,
      statements,
    } => ASTNode::SemanticBlock {
      node_id: node_id,
      src_location: src_location,
      documentation: documentation,
      statements: statements.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::Break {
      node_id,
      src_location,
    } => ASTNode::Break {
      node_id: node_id,
      src_location: src_location,
    },
    ASTNode::Continue {
      node_id,
      src_location,
    } => ASTNode::Continue {
      node_id: node_id,
      src_location: src_location,
    },
    ASTNode::DoWhileStatement {
      node_id,
      src_location,
      nodes,
      body,
    } => ASTNode::DoWhileStatement {
      node_id: node_id,
      src_location: src_location,
      nodes: nodes.iter().map(|n| node_to_stub(n)).collect(),
      body: match body {
        Some(b) => Some(Box::new(node_to_stub(&b))),
        None => None,
      },
    },
    ASTNode::EmitStatement {
      node_id,
      src_location,
      event_call,
    } => ASTNode::EmitStatement {
      node_id: node_id,
      src_location: src_location,
      event_call: Box::new(node_to_stub(&event_call)),
    },
    ASTNode::ExpressionStatement {
      node_id,
      src_location,
      expression,
    } => ASTNode::ExpressionStatement {
      node_id: node_id,
      src_location: src_location,
      expression: Box::new(node_to_stub(&expression)),
    },
    ASTNode::ForStatement {
      node_id,
      src_location,
      body,
      condition,
      initialization_expression,
      is_simple_counter_loop,
      loop_expression,
    } => ASTNode::ForStatement {
      node_id: node_id,
      src_location: src_location,
      body: Box::new(node_to_stub(&body)),
      condition: match condition {
        Some(c) => Some(Box::new(node_to_stub(&c))),
        None => None,
      },
      initialization_expression: match initialization_expression {
        Some(ie) => Some(Box::new(node_to_stub(&ie))),
        None => None,
      },
      is_simple_counter_loop: is_simple_counter_loop,
      loop_expression: match loop_expression {
        Some(le) => Some(Box::new(node_to_stub(&le))),
        None => None,
      },
    },
    ASTNode::IfStatement {
      node_id,
      src_location,
      condition,
      true_body,
      false_body,
    } => ASTNode::IfStatement {
      node_id: node_id,
      src_location: src_location,
      condition: Box::new(node_to_stub(&condition)),
      true_body: Box::new(node_to_stub(&true_body)),
      false_body: match false_body {
        Some(fb) => Some(Box::new(node_to_stub(&fb))),
        None => None,
      },
    },
    ASTNode::InlineAssembly {
      node_id,
      src_location,
    } => ASTNode::InlineAssembly {
      node_id: node_id,
      src_location: src_location,
    },
    ASTNode::PlaceholderStatement {
      node_id,
      src_location,
    } => ASTNode::PlaceholderStatement {
      node_id: node_id,
      src_location: src_location,
    },
    ASTNode::Return {
      node_id,
      src_location,
      expression,
      function_return_parameters,
    } => ASTNode::Return {
      node_id: node_id,
      src_location: src_location,
      expression: match expression {
        Some(e) => Some(Box::new(node_to_stub(&e))),
        None => None,
      },
      function_return_parameters: function_return_parameters,
    },
    ASTNode::RevertStatement {
      node_id,
      src_location,
      error_call,
    } => ASTNode::RevertStatement {
      node_id: node_id,
      src_location: src_location,
      error_call: Box::new(node_to_stub(&error_call)),
    },
    ASTNode::TryStatement {
      node_id,
      src_location,
      clauses,
      external_call,
    } => ASTNode::TryStatement {
      node_id: node_id,
      src_location: src_location,
      clauses: clauses.iter().map(|n| node_to_stub(n)).collect(),
      external_call: Box::new(node_to_stub(&external_call)),
    },
    ASTNode::UncheckedBlock {
      node_id,
      src_location,
      statements,
    } => ASTNode::UncheckedBlock {
      node_id: node_id,
      src_location: src_location,
      statements: statements.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::VariableDeclarationStatement {
      node_id,
      src_location,
      declarations,
      initial_value,
    } => ASTNode::VariableDeclarationStatement {
      node_id: node_id,
      src_location: src_location,
      declarations: declarations.iter().map(|n| node_to_stub(n)).collect(),
      initial_value: match initial_value {
        Some(iv) => Some(Box::new(node_to_stub(&iv))),
        None => None,
      },
    },
    ASTNode::VariableDeclaration {
      node_id,
      src_location,
      constant,
      function_selector,
      mutability,
      name,
      name_location,
      scope,
      state_variable,
      storage_location,
      type_name,
      value,
      visibility,
    } => ASTNode::VariableDeclaration {
      node_id: node_id,
      src_location: src_location,
      constant: constant,
      function_selector: function_selector,
      mutability: mutability,
      name: name,
      name_location: name_location,
      scope: scope,
      state_variable: state_variable,
      storage_location: storage_location,
      type_name: Box::new(node_to_stub(&type_name)),
      value: match value {
        Some(v) => Some(Box::new(node_to_stub(&v))),
        None => None,
      },
      visibility: visibility,
    },
    ASTNode::WhileStatement {
      node_id,
      src_location,
      condition,
      body,
    } => ASTNode::WhileStatement {
      node_id: node_id,
      src_location: src_location,
      condition: Box::new(node_to_stub(&condition)),
      body: match body {
        Some(b) => Some(Box::new(node_to_stub(&b))),
        None => None,
      },
    },
    ASTNode::ContractDefinition {
      node_id,
      src_location,
      nodes,
      abstract_,
      base_contracts,
      name,
      name_location,
      contract_kind,
    } => ASTNode::ContractDefinition {
      node_id: node_id,
      src_location: src_location,
      nodes: nodes.iter().map(|n| node_to_stub(n)).collect(),
      abstract_: abstract_,
      base_contracts: base_contracts.iter().map(|n| node_to_stub(n)).collect(),
      name: name,
      name_location: name_location,
      contract_kind: contract_kind,
    },
    ASTNode::FunctionDefinition {
      node_id,
      src_location,
      body,
      documentation,
      implemented,
      kind,
      modifiers,
      name,
      name_location,
      parameters,
      return_parameters,
      scope,
      state_mutability,
      virtual_,
      visibility,
    } => ASTNode::FunctionDefinition {
      node_id: node_id,
      src_location: src_location,
      body: match body {
        Some(b) => Some(Box::new(node_to_stub(&b))),
        None => None,
      },
      documentation: match documentation {
        Some(d) => Some(Box::new(node_to_stub(&d))),
        None => None,
      },
      implemented: implemented,
      kind: kind,
      modifiers: modifiers.iter().map(|n| node_to_stub(n)).collect(),
      name: name,
      name_location: name_location,
      parameters: Box::new(node_to_stub(&parameters)),
      return_parameters: Box::new(node_to_stub(&return_parameters)),
      scope: scope,
      state_mutability: state_mutability,
      virtual_: virtual_,
      visibility: visibility,
    },
    ASTNode::EventDefinition {
      node_id,
      src_location,
      name,
      name_location,
      parameters,
    } => ASTNode::EventDefinition {
      node_id: node_id,
      src_location: src_location,
      name: name,
      name_location: name_location,
      parameters: Box::new(node_to_stub(&parameters)),
    },
    ASTNode::ErrorDefinition {
      node_id,
      src_location,
      name,
      name_location,
      parameters,
    } => ASTNode::ErrorDefinition {
      node_id: node_id,
      src_location: src_location,
      name: name,
      name_location: name_location,
      parameters: Box::new(node_to_stub(&parameters)),
    },
    ASTNode::ModifierDefinition {
      node_id,
      src_location,
      body,
      documentation,
      name,
      name_location,
      parameters,
      virtual_,
      visibility,
    } => ASTNode::ModifierDefinition {
      node_id: node_id,
      src_location: src_location,
      body: Box::new(node_to_stub(&body)),
      documentation: match documentation {
        Some(d) => Some(Box::new(node_to_stub(&d))),
        None => None,
      },
      name: name,
      name_location: name_location,
      parameters: Box::new(node_to_stub(&parameters)),
      virtual_: virtual_,
      visibility: visibility,
    },
    ASTNode::StructDefinition {
      node_id,
      src_location,
      members,
      canonical_name,
      name,
      name_location,
      visibility,
    } => ASTNode::StructDefinition {
      node_id: node_id,
      src_location: src_location,
      members: members.iter().map(|n| node_to_stub(n)).collect(),
      canonical_name: canonical_name,
      name: name,
      name_location: name_location,
      visibility: visibility,
    },
    ASTNode::EnumDefinition {
      node_id,
      src_location,
      members,
      canonical_name,
      name,
      name_location,
    } => ASTNode::EnumDefinition {
      node_id: node_id,
      src_location: src_location,
      members: members.iter().map(|n| node_to_stub(n)).collect(),
      canonical_name: canonical_name,
      name: name,
      name_location: name_location,
    },
    ASTNode::UserDefinedValueTypeDefinition {
      node_id,
      src_location,
      name,
      underlying_type,
    } => ASTNode::UserDefinedValueTypeDefinition {
      node_id: node_id,
      src_location: src_location,
      name: name,
      underlying_type: Box::new(node_to_stub(&underlying_type)),
    },
    ASTNode::PragmaDirective {
      node_id,
      src_location,
      literals,
    } => ASTNode::PragmaDirective {
      node_id: node_id,
      src_location: src_location,
      literals: literals,
    },
    ASTNode::ImportDirective {
      node_id,
      src_location,
      absolute_path,
      file,
      source_unit,
    } => ASTNode::ImportDirective {
      node_id: node_id,
      src_location: src_location,
      absolute_path: absolute_path,
      file: file,
      source_unit: source_unit,
    },
    ASTNode::UsingForDirective {
      node_id,
      src_location,
      global,
      library_name,
      type_name,
    } => ASTNode::UsingForDirective {
      node_id: node_id,
      src_location: src_location,
      global: global,
      library_name: match library_name {
        Some(ln) => Some(Box::new(node_to_stub(&ln))),
        None => None,
      },
      type_name: match type_name {
        Some(tn) => Some(Box::new(node_to_stub(&tn))),
        None => None,
      },
    },
    ASTNode::SourceUnit {
      node_id,
      src_location,
      nodes,
    } => ASTNode::SourceUnit {
      node_id: node_id,
      src_location: src_location,
      nodes: nodes.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::InheritanceSpecifier {
      node_id,
      src_location,
      base_name,
    } => ASTNode::InheritanceSpecifier {
      node_id: node_id,
      src_location: src_location,
      base_name: Box::new(node_to_stub(&base_name)),
    },
    ASTNode::ElementaryTypeName {
      node_id,
      src_location,
      name,
    } => ASTNode::ElementaryTypeName {
      node_id: node_id,
      src_location: src_location,
      name: name,
    },
    ASTNode::FunctionTypeName {
      node_id,
      src_location,
      parameter_types,
      return_parameter_types,
      state_mutability,
      visibility,
    } => ASTNode::FunctionTypeName {
      node_id: node_id,
      src_location: src_location,
      parameter_types: Box::new(node_to_stub(&parameter_types)),
      return_parameter_types: Box::new(node_to_stub(&return_parameter_types)),
      state_mutability: state_mutability,
      visibility: visibility,
    },
    ASTNode::ParameterList {
      node_id,
      src_location,
      parameters,
    } => ASTNode::ParameterList {
      node_id: node_id,
      src_location: src_location,
      parameters: parameters.iter().map(|n| node_to_stub(n)).collect(),
    },
    ASTNode::TryCatchClause {
      node_id,
      src_location,
      error_name,
      block,
      parameters,
    } => ASTNode::TryCatchClause {
      node_id: node_id,
      src_location: src_location,
      error_name: error_name,
      block: Box::new(node_to_stub(&block)),
      parameters: match parameters {
        Some(p) => Some(Box::new(node_to_stub(&p))),
        None => None,
      },
    },
    ASTNode::ModifierInvocation {
      node_id,
      src_location,
      modifier_name,
      arguments,
    } => ASTNode::ModifierInvocation {
      node_id: node_id,
      src_location: src_location,
      modifier_name: Box::new(node_to_stub(&modifier_name)),
      arguments: match arguments {
        Some(args) => Some(args.iter().map(|n| node_to_stub(n)).collect()),
        None => None,
      },
    },
    ASTNode::UserDefinedTypeName {
      node_id,
      src_location,
      path_node,
      referenced_declaration,
    } => ASTNode::UserDefinedTypeName {
      node_id: node_id,
      src_location: src_location,
      path_node: Box::new(node_to_stub(&path_node)),
      referenced_declaration: referenced_declaration,
    },
    ASTNode::ArrayTypeName {
      node_id,
      src_location,
      base_type,
    } => ASTNode::ArrayTypeName {
      node_id: node_id,
      src_location: src_location,
      base_type: Box::new(node_to_stub(&base_type)),
    },
    ASTNode::Mapping {
      node_id,
      src_location,
      key_name,
      key_name_location,
      key_type,
      value_name,
      value_name_location,
      value_type,
    } => ASTNode::Mapping {
      node_id: node_id,
      src_location: src_location,
      key_name: key_name,
      key_name_location: key_name_location,
      key_type: Box::new(node_to_stub(&key_type)),
      value_name: value_name,
      value_name_location: value_name_location,
      value_type: Box::new(node_to_stub(&value_type)),
    },
    ASTNode::StructuredDocumentation {
      node_id,
      src_location,
      text,
    } => ASTNode::StructuredDocumentation {
      node_id: node_id,
      src_location: src_location,
      text: text,
    },
    ASTNode::Stub {
      node_id,
      src_location,
      topic,
    } => ASTNode::Stub {
      node_id: node_id,
      src_location: src_location,
      topic: topic,
    },
    ASTNode::Other {
      node_id,
      src_location,
      nodes,
      body,
      node_type,
    } => ASTNode::Other {
      node_id: node_id,
      src_location: src_location,
      nodes: nodes.iter().map(|n| node_to_stub(n)).collect(),
      body: match body {
        Some(b) => Some(Box::new(node_to_stub(&b))),
        None => None,
      },
      node_type: node_type,
    },
  }
}

// Helper functions for JSON parsing
fn get_required_i32(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<i32, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_i64())
    .map(|v| v as i32)
    .ok_or_else(|| {
      let available_fields: Vec<&str> = val
        .as_object()
        .map(|obj| obj.keys().map(|k| k.as_str()).collect())
        .unwrap_or_default();
      format!(
        "Missing or invalid {} field. Available fields: {:?}",
        field_name, available_fields
      )
    })
}

// Helper functions with node type context for better error messages
fn get_required_i32_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<i32, String> {
  get_required_i32(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_string_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<String, String> {
  get_required_string(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_optional_string_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<Option<String>, String> {
  match val.get(field_name) {
    Some(v) => {
      if v.is_null() {
        Ok(None)
      } else {
        v.as_str()
          .map(|s| {
            if s.is_empty() {
              None
            } else {
              Some(s.to_string())
            }
          })
          .ok_or_else(|| {
            format!(
              "Error parsing {} node: Invalid {} field type",
              node_type, field_name
            )
          })
      }
    }
    None => Ok(None),
  }
}

fn get_required_bool_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<bool, String> {
  get_required_bool(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_source_location_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<SourceLocation, String> {
  get_required_source_location(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_enum_with_context<T: FromStr>(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<T, String>
where
  T::Err: std::fmt::Debug,
{
  get_required_enum(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_node_vec_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
  source_content: &str,
) -> Result<Vec<ASTNode>, String> {
  get_required_node_vec(val, field_name, source_content)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_optional_node_vec_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
  source_content: &str,
) -> Result<Option<Vec<ASTNode>>, String> {
  get_optional_node_vec(val, field_name, source_content)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_node_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
  source_content: &str,
) -> Result<Box<ASTNode>, String> {
  get_required_node(val, field_name, source_content)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_optional_node_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
  source_content: &str,
) -> Result<Option<Box<ASTNode>>, String> {
  match val.get(field_name) {
    Some(v) => {
      if v.is_null() {
        Ok(None)
      } else {
        node_from_json(v, source_content)
          .map(|node| Some(Box::new(node)))
          .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
      }
    }
    None => Ok(None),
  }
}

fn get_required_string_vec_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<Vec<String>, String> {
  get_required_string_vec(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_i32_vec_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<Vec<i32>, String> {
  get_required_i32_vec(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_source_location_vec_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<Vec<SourceLocation>, String> {
  get_required_source_location_vec(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_type_descriptions_with_context(
  val: &serde_json::Value,
  field_name: &str,
  node_type: &str,
) -> Result<TypeDescriptions, String> {
  get_required_type_descriptions(val, field_name)
    .map_err(|e| format!("Error parsing {} node: {}", node_type, e))
}

fn get_required_string(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<String, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_str())
    .map(|s| s.to_string())
    .ok_or_else(|| {
      let available_fields: Vec<&str> = val
        .as_object()
        .map(|obj| obj.keys().map(|k| k.as_str()).collect())
        .unwrap_or_default();
      format!(
        "Missing or invalid {} field. Available fields: {:?}",
        field_name, available_fields
      )
    })
}

fn get_required_bool(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<bool, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_bool())
    .ok_or_else(|| {
      let available_fields: Vec<&str> = val
        .as_object()
        .map(|obj| obj.keys().map(|k| k.as_str()).collect())
        .unwrap_or_default();
      format!(
        "Missing or invalid {} field. Available fields: {:?}",
        field_name, available_fields
      )
    })
}

fn get_required_source_location(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<SourceLocation, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_str())
    .ok_or_else(|| format!("Missing {} field: {:?}", field_name, val))
    .and_then(|v| SourceLocation::from_str(v))
}

fn get_required_enum<T: FromStr>(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<T, String>
where
  T::Err: std::fmt::Debug,
{
  val
    .get(field_name)
    .and_then(|v| v.as_str())
    .ok_or_else(|| {
      let available_fields: Vec<&str> = val
        .as_object()
        .map(|obj| obj.keys().map(|k| k.as_str()).collect())
        .unwrap_or_default();
      format!(
        "Missing {} field. Available fields: {:?}",
        field_name, available_fields
      )
    })
    .and_then(|s| {
      s.parse()
        .map_err(|e| format!("Failed to parse {} '{}': {:?}", field_name, s, e))
    })
}

fn get_required_node_vec(
  val: &serde_json::Value,
  field_name: &str,
  source_content: &str,
) -> Result<Vec<ASTNode>, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_array())
    .ok_or_else(|| {
      format!("Missing or invalid {} field: {:?}", field_name, val)
    })
    .and_then(|arr| {
      let filtered: Vec<_> =
        arr.iter().filter(|item| !item.is_null()).collect();

      filtered
        .into_iter()
        .map(|item| node_from_json(item, source_content))
        .collect::<Result<Vec<ASTNode>, String>>()
    })
}

fn get_optional_node_vec(
  val: &serde_json::Value,
  field_name: &str,
  source_content: &str,
) -> Result<Option<Vec<ASTNode>>, String> {
  match val.get(field_name) {
    Some(v) => {
      if v.is_null() {
        Ok(None)
      } else {
        v.as_array()
          .ok_or_else(|| format!("Field '{}' is not an array", field_name))
          .and_then(|arr| {
            let filtered: Vec<_> =
              arr.iter().filter(|item| !item.is_null()).collect();

            filtered
              .into_iter()
              .map(|item| node_from_json(item, source_content))
              .collect::<Result<Vec<ASTNode>, String>>()
              .map(Some)
          })
      }
    }
    None => Ok(None),
  }
}

fn get_required_node(
  val: &serde_json::Value,
  field_name: &str,
  source_content: &str,
) -> Result<Box<ASTNode>, String> {
  let available_fields: Vec<String> = val
    .as_object()
    .unwrap_or(&serde_json::Map::new())
    .keys()
    .cloned()
    .collect();

  val
    .get(field_name)
    .ok_or_else(|| {
      format!(
        "Missing {} field. Available fields: {:?}",
        field_name, available_fields
      )
    })
    .and_then(|v| node_from_json(v, source_content))
    .map(Box::new)
}

fn get_optional_node(
  val: &serde_json::Value,
  field_name: &str,
  source_content: &str,
) -> Result<Option<Box<ASTNode>>, String> {
  match val.get(field_name) {
    Some(v) => {
      if v.is_null() {
        Ok(None)
      } else {
        node_from_json(v, source_content).map(|node| Some(Box::new(node)))
      }
    }
    None => Ok(None),
  }
}

fn get_required_string_vec(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<Vec<String>, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_array())
    .ok_or_else(|| {
      format!("Missing or invalid {} field: {:?}", field_name, val)
    })
    .and_then(|arr| {
      arr
        .iter()
        .map(|item| {
          item.as_str().map(|s| s.to_string()).ok_or_else(|| {
            format!("Invalid string in {} array: {:?}", field_name, item)
          })
        })
        .collect::<Result<Vec<String>, String>>()
    })
}

fn get_required_i32_vec(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<Vec<i32>, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_array())
    .ok_or_else(|| {
      format!("Missing or invalid {} field: {:?}", field_name, val)
    })
    .and_then(|arr| {
      arr
        .iter()
        .map(|item| {
          item.as_i64().map(|i| i as i32).ok_or_else(|| {
            format!("Invalid integer in {} array: {:?}", field_name, item)
          })
        })
        .collect::<Result<Vec<i32>, String>>()
    })
}

fn get_required_source_location_vec(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<Vec<SourceLocation>, String> {
  val
    .get(field_name)
    .and_then(|v| v.as_array())
    .ok_or_else(|| {
      format!("Missing or invalid {} field: {:?}", field_name, val)
    })
    .and_then(|arr| {
      arr
        .iter()
        .map(|item| {
          item
            .as_str()
            .ok_or_else(|| {
              format!(
                "Invalid source location in {} array: {:?}",
                field_name, item
              )
            })
            .and_then(|s| SourceLocation::from_str(s))
        })
        .collect::<Result<Vec<SourceLocation>, String>>()
    })
}

fn get_required_type_descriptions(
  val: &serde_json::Value,
  field_name: &str,
) -> Result<TypeDescriptions, String> {
  val
    .get(field_name)
    .ok_or_else(|| format!("Missing {} field: {:?}", field_name, val))
    .and_then(|v| TypeDescriptions::from_json(v))
}

fn find_semantic_breaks(source: &str, statements: &[ASTNode]) -> Vec<usize> {
  let mut breaks = vec![];

  if statements.len() <= 1 {
    return breaks;
  }

  for i in 0..statements.len() - 1 {
    let current_end = statements[i].src_location().start.unwrap_or(0)
      + statements[i].src_location().length.unwrap_or(0);
    let next_start = statements[i + 1].src_location().start.unwrap_or(0);

    if current_end < next_start {
      let between_text = &source[current_end..next_start];
      let newline_count = between_text.matches('\n').count();

      // Two or more newlines indicate a semantic break
      if newline_count >= 2 {
        breaks.push(i + 1);
      }
    }
  }

  breaks
}

fn extract_block_documentation(
  source: &str,
  group_start: usize,
  first_statement_start: usize,
) -> Option<String> {
  if group_start >= first_statement_start {
    return None;
  }

  let text_before = &source[group_start..first_statement_start];
  let lines: Vec<&str> = text_before.lines().collect();
  let mut doc_lines = vec![];

  for line in lines {
    let trimmed = line.trim();
    if trimmed.starts_with("//") {
      doc_lines.push(trimmed.trim_start_matches("//").trim());
    } else if trimmed.starts_with("/*") && trimmed.ends_with("*/") {
      let content = trimmed
        .trim_start_matches("/*")
        .trim_end_matches("*/")
        .trim();
      doc_lines.push(content);
    } else if !trimmed.is_empty() && !doc_lines.is_empty() {
      // Non-comment, non-empty line breaks the documentation block
      break;
    }
  }

  if doc_lines.is_empty() {
    None
  } else {
    Some(doc_lines.join("\n"))
  }
}

static mut NEXT_GENERATED_NODE_ID: i32 = -100; // Run negative to avoid conflicts

fn generate_node_id() -> i32 {
  unsafe {
    let id = NEXT_GENERATED_NODE_ID;
    NEXT_GENERATED_NODE_ID -= 1;
    id
  }
}

fn group_statements_into_semantic_blocks(
  statements: Vec<ASTNode>,
  source: &str,
  block_src_location: &SourceLocation,
) -> Result<Vec<ASTNode>, String> {
  if statements.is_empty() {
    return Ok(vec![]);
  }

  let breaks = find_semantic_breaks(source, &statements);
  let mut semantic_blocks = vec![];
  let mut current_group_start = 0;

  let mut create_semantic_block =
    |group_statements: Vec<ASTNode>, group_start_index: usize| {
      if group_statements.is_empty() {
        return;
      }

      let first_stmt_start =
        group_statements[0].src_location().start.unwrap_or(0);
      let last_stmt = &group_statements[group_statements.len() - 1];
      let last_stmt_end = last_stmt.src_location().start.unwrap_or(0)
        + last_stmt.src_location().length.unwrap_or(0);

      let group_start_pos = if group_start_index == 0 {
        block_src_location.start.unwrap_or(0)
      } else {
        statements[group_start_index - 1]
          .src_location()
          .start
          .unwrap_or(0)
          + statements[group_start_index - 1]
            .src_location()
            .length
            .unwrap_or(0)
      };

      let documentation =
        extract_block_documentation(source, group_start_pos, first_stmt_start);

      let semantic_block = ASTNode::SemanticBlock {
        node_id: generate_node_id(),
        src_location: SourceLocation {
          start: Some(first_stmt_start),
          length: Some(last_stmt_end - first_stmt_start),
          index: None,
        },
        documentation,
        statements: group_statements,
      };

      semantic_blocks.push(semantic_block);
    };

  for &break_index in &breaks {
    let group_statements =
      statements[current_group_start..break_index].to_vec();
    create_semantic_block(group_statements, current_group_start);
    current_group_start = break_index;
  }

  // Handle the last group
  let group_statements = statements[current_group_start..].to_vec();
  create_semantic_block(group_statements, current_group_start);

  Ok(semantic_blocks)
}

pub fn node_from_json(
  val: &serde_json::Value,
  source_content: &str,
) -> Result<ASTNode, String> {
  // Handle null values - they should not reach here for required fields
  // but this provides a clear error if they do
  if val.is_null() {
    return Err(
      "Cannot parse null node value - this indicates a required field is null"
        .to_string(),
    );
  }

  let node_type_str = val
    .get("nodeType")
    .and_then(|v| v.as_str())
    .ok_or_else(|| format!("Missing nodeType field: {:?}", val))?;

  let node_id = get_required_i32(val, "id")
    .map_err(|e| format!("Error parsing {} node: {}", node_type_str, e))?;
  let src_location = val
    .get("src")
    .and_then(|v| v.as_str())
    .ok_or_else(|| {
      format!("Missing src field in {} node: {:?}", node_type_str, val)
    })
    .and_then(|v| SourceLocation::from_str(v))
    .map_err(|e| format!("Error parsing {} node: {}", node_type_str, e))?;

  match node_type_str {
    "Assignment" => {
      let operator =
        get_required_enum_with_context(val, "operator", node_type_str)?;
      let right_hand_side = get_required_node_with_context(
        val,
        "rightHandSide",
        node_type_str,
        source_content,
      )?;
      let left_hand_side = get_required_node_with_context(
        val,
        "leftHandSide",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::Assignment {
        node_id,
        src_location,
        operator,
        right_hand_side,
        left_hand_side,
      })
    }
    "BinaryOperation" => {
      let left_expression = get_required_node_with_context(
        val,
        "leftExpression",
        node_type_str,
        source_content,
      )?;
      let operator =
        get_required_enum_with_context(val, "operator", node_type_str)?;
      let right_expression = get_required_node_with_context(
        val,
        "rightExpression",
        node_type_str,
        source_content,
      )?;
      let type_descriptions = get_required_type_descriptions_with_context(
        val,
        "typeDescriptions",
        "BinaryOperation",
      )?;

      Ok(ASTNode::BinaryOperation {
        node_id,
        src_location,
        left_expression,
        operator,
        right_expression,
        type_descriptions,
      })
    }
    "Conditional" => {
      let condition = get_required_node_with_context(
        val,
        "condition",
        node_type_str,
        source_content,
      )?;
      let true_expression = get_required_node_with_context(
        val,
        "trueExpression",
        node_type_str,
        source_content,
      )?;
      let false_expression = get_optional_node_with_context(
        val,
        "falseExpression",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::Conditional {
        node_id,
        src_location,
        condition,
        true_expression,
        false_expression,
      })
    }
    "ElementaryTypeNameExpression" => {
      let type_descriptions = get_required_type_descriptions_with_context(
        val,
        "typeDescriptions",
        node_type_str,
      )?;
      let type_name = get_required_node_with_context(
        val,
        "typeName",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ElementaryTypeNameExpression {
        node_id,
        src_location,
        type_descriptions,
        type_name,
      })
    }
    "FunctionCall" => {
      let arguments = get_required_node_vec_with_context(
        val,
        "arguments",
        node_type_str,
        source_content,
      )?;
      let expression = get_required_node_with_context(
        val,
        "expression",
        node_type_str,
        source_content,
      )?;
      let kind = get_required_enum_with_context(val, "kind", node_type_str)?;
      let name_locations = get_required_source_location_vec_with_context(
        val,
        "nameLocations",
        node_type_str,
      )?;
      let names =
        get_required_string_vec_with_context(val, "names", node_type_str)?;
      let try_call =
        get_required_bool_with_context(val, "tryCall", node_type_str)?;
      let type_descriptions = get_required_type_descriptions_with_context(
        val,
        "typeDescriptions",
        node_type_str,
      )?;

      Ok(ASTNode::FunctionCall {
        node_id,
        src_location,
        arguments,
        expression,
        kind,
        name_locations,
        names,
        try_call,
        type_descriptions,
      })
    }
    "FunctionCallOptions" => {
      let expression = get_required_node_with_context(
        val,
        "expression",
        node_type_str,
        source_content,
      )?;
      let options = get_required_node_vec_with_context(
        val,
        "options",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::FunctionCallOptions {
        node_id,
        src_location,
        expression,
        options,
      })
    }
    "Identifier" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let overloaded_declarations = get_required_i32_vec_with_context(
        val,
        "overloadedDeclarations",
        node_type_str,
      )?;
      let referenced_declaration = get_required_i32_with_context(
        val,
        "referencedDeclaration",
        node_type_str,
      )?;
      let type_descriptions = get_required_type_descriptions_with_context(
        val,
        "typeDescriptions",
        node_type_str,
      )?;

      Ok(ASTNode::Identifier {
        node_id,
        src_location,
        name,
        overloaded_declarations,
        referenced_declaration,
        type_descriptions,
      })
    }
    "IdentifierPath" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_locations = get_required_source_location_vec_with_context(
        val,
        "nameLocations",
        node_type_str,
      )?;
      let referenced_declaration = get_required_i32_with_context(
        val,
        "referencedDeclaration",
        node_type_str,
      )?;

      Ok(ASTNode::IdentifierPath {
        node_id,
        src_location,
        name,
        name_locations,
        referenced_declaration,
      })
    }
    "IndexAccess" => {
      let base_expression = get_required_node_with_context(
        val,
        "baseExpression",
        node_type_str,
        source_content,
      )?;
      let index_expression = get_optional_node_with_context(
        val,
        "indexExpression",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::IndexAccess {
        node_id,
        src_location,
        base_expression,
        index_expression,
      })
    }
    "IndexRangeAccess" => {
      let nodes = get_required_node_vec_with_context(
        val,
        "nodes",
        node_type_str,
        source_content,
      )?;
      let body = get_optional_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::IndexRangeAccess {
        node_id,
        src_location,
        nodes,
        body,
      })
    }
    "Literal" => {
      let hex_value =
        get_required_string_with_context(val, "hexValue", node_type_str)?;
      let kind = get_required_enum_with_context(val, "kind", node_type_str)?;
      let type_descriptions = get_required_type_descriptions_with_context(
        val,
        "typeDescriptions",
        node_type_str,
      )?;
      let value =
        get_optional_string_with_context(val, "value", node_type_str)?;

      Ok(ASTNode::Literal {
        node_id,
        src_location,
        hex_value,
        kind,
        type_descriptions,
        value,
      })
    }
    "MemberAccess" => {
      let expression = get_required_node_with_context(
        val,
        "expression",
        node_type_str,
        source_content,
      )?;
      let member_location = get_required_source_location_with_context(
        val,
        "memberLocation",
        node_type_str,
      )?;
      let member_name =
        get_required_string_with_context(val, "memberName", node_type_str)?;

      Ok(ASTNode::MemberAccess {
        node_id,
        src_location,
        expression,
        member_location,
        member_name,
      })
    }
    "NewExpression" => {
      let type_name = get_required_node_with_context(
        val,
        "typeName",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::NewExpression {
        node_id,
        src_location,
        type_name,
      })
    }
    "TupleExpression" => {
      let components = get_required_node_vec_with_context(
        val,
        "components",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::TupleExpression {
        node_id,
        src_location,
        components,
      })
    }
    "UnaryOperation" => {
      let prefix =
        get_required_bool_with_context(val, "prefix", node_type_str)?;
      let operator =
        get_required_enum_with_context(val, "operator", node_type_str)?;
      let sub_expression = get_required_node_with_context(
        val,
        "subExpression",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::UnaryOperation {
        node_id,
        src_location,
        prefix,
        operator,
        sub_expression,
      })
    }
    "EnumValue" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;

      Ok(ASTNode::EnumValue {
        node_id,
        src_location,
        name,
        name_location,
      })
    }
    "Block" => {
      let statements = get_required_node_vec_with_context(
        val,
        "statements",
        node_type_str,
        source_content,
      )?;

      // Transform statements into semantic blocks
      let semantic_blocks = if !statements.is_empty() {
        group_statements_into_semantic_blocks(
          statements,
          source_content,
          &src_location,
        )?
      } else {
        vec![]
      };

      Ok(ASTNode::Block {
        node_id,
        src_location,
        statements: semantic_blocks,
      })
    }
    "Break" => Ok(ASTNode::Break {
      node_id,
      src_location,
    }),
    "Continue" => Ok(ASTNode::Continue {
      node_id,
      src_location,
    }),
    "DoWhileStatement" => {
      let nodes = get_required_node_vec_with_context(
        val,
        "nodes",
        node_type_str,
        source_content,
      )?;
      let body = get_optional_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::DoWhileStatement {
        node_id,
        src_location,
        nodes,
        body,
      })
    }
    "EmitStatement" => {
      let event_call = get_required_node_with_context(
        val,
        "eventCall",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::EmitStatement {
        node_id,
        src_location,
        event_call,
      })
    }
    "ExpressionStatement" => {
      let expression = get_required_node_with_context(
        val,
        "expression",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ExpressionStatement {
        node_id,
        src_location,
        expression,
      })
    }
    "ForStatement" => {
      let body = get_required_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;
      let condition = get_optional_node_with_context(
        val,
        "condition",
        node_type_str,
        source_content,
      )?;
      let initialization_expression = get_optional_node_with_context(
        val,
        "initializationExpression",
        node_type_str,
        source_content,
      )?;
      let is_simple_counter_loop = get_required_bool_with_context(
        val,
        "isSimpleCounterLoop",
        node_type_str,
      )?;
      let loop_expression = get_optional_node_with_context(
        val,
        "loopExpression",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ForStatement {
        node_id,
        src_location,
        body,
        condition,
        initialization_expression,
        is_simple_counter_loop,
        loop_expression,
      })
    }
    "IfStatement" => {
      let condition = get_required_node_with_context(
        val,
        "condition",
        node_type_str,
        source_content,
      )?;
      let true_body = get_required_node_with_context(
        val,
        "trueBody",
        node_type_str,
        source_content,
      )?;
      let false_body = get_optional_node_with_context(
        val,
        "falseBody",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::IfStatement {
        node_id,
        src_location,
        condition,
        true_body,
        false_body,
      })
    }
    "InlineAssembly" => Ok(ASTNode::InlineAssembly {
      node_id,
      src_location,
    }),
    "PlaceholderStatement" => Ok(ASTNode::PlaceholderStatement {
      node_id,
      src_location,
    }),
    "Return" => {
      let expression = get_optional_node_with_context(
        val,
        "expression",
        node_type_str,
        source_content,
      )?;
      let function_return_parameters = get_required_i32_with_context(
        val,
        "functionReturnParameters",
        node_type_str,
      )?;

      Ok(ASTNode::Return {
        node_id,
        src_location,
        expression,
        function_return_parameters,
      })
    }
    "RevertStatement" => {
      let error_call = get_required_node_with_context(
        val,
        "errorCall",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::RevertStatement {
        node_id,
        src_location,
        error_call,
      })
    }
    "TryStatement" => {
      let clauses = get_required_node_vec_with_context(
        val,
        "clauses",
        node_type_str,
        source_content,
      )?;
      let external_call = get_required_node_with_context(
        val,
        "externalCall",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::TryStatement {
        node_id,
        src_location,
        clauses,
        external_call,
      })
    }
    "UncheckedBlock" => {
      let statements = get_required_node_vec_with_context(
        val,
        "statements",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::UncheckedBlock {
        node_id,
        src_location,
        statements,
      })
    }
    "VariableDeclarationStatement" => {
      let declarations = get_required_node_vec_with_context(
        val,
        "declarations",
        node_type_str,
        source_content,
      )?;
      let initial_value = get_optional_node_with_context(
        val,
        "initialValue",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::VariableDeclarationStatement {
        node_id,
        src_location,
        declarations,
        initial_value,
      })
    }
    "VariableDeclaration" => {
      let constant =
        get_required_bool_with_context(val, "constant", node_type_str)?;
      let function_selector = get_optional_string_with_context(
        val,
        "functionSelector",
        node_type_str,
      )?;
      let mutability =
        get_required_enum_with_context(val, "mutability", node_type_str)?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let scope = get_required_i32_with_context(val, "scope", node_type_str)?;
      let state_variable =
        get_required_bool_with_context(val, "stateVariable", node_type_str)?;
      let storage_location =
        get_required_enum_with_context(val, "storageLocation", node_type_str)?;
      let type_name = get_required_node_with_context(
        val,
        "typeName",
        node_type_str,
        source_content,
      )?;
      let value = get_optional_node_with_context(
        val,
        "value",
        node_type_str,
        source_content,
      )?;
      let visibility =
        get_required_enum_with_context(val, "visibility", node_type_str)?;

      Ok(ASTNode::VariableDeclaration {
        node_id,
        src_location,
        constant,
        function_selector,
        mutability,
        name,
        name_location,
        scope,
        state_variable,
        storage_location,
        type_name,
        value,
        visibility,
      })
    }
    "WhileStatement" => {
      let condition = get_required_node_with_context(
        val,
        "condition",
        node_type_str,
        source_content,
      )?;
      let body = get_optional_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::WhileStatement {
        node_id,
        src_location,
        condition,
        body,
      })
    }
    "ContractDefinition" => {
      let nodes = get_required_node_vec_with_context(
        val,
        "nodes",
        node_type_str,
        source_content,
      )?;
      let abstract_ =
        get_required_bool_with_context(val, "abstract", node_type_str)?;
      let base_contracts = get_required_node_vec_with_context(
        val,
        "baseContracts",
        node_type_str,
        source_content,
      )?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let contract_kind =
        get_required_enum_with_context(val, "contractKind", node_type_str)?;

      Ok(ASTNode::ContractDefinition {
        node_id,
        src_location,
        nodes,
        abstract_,
        base_contracts,
        name,
        name_location,
        contract_kind,
      })
    }
    "FunctionDefinition" => {
      let body = get_optional_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;
      let documentation = get_optional_node_with_context(
        val,
        "documentation",
        node_type_str,
        source_content,
      )?;
      let implemented =
        get_required_bool_with_context(val, "implemented", node_type_str)?;
      let kind = get_required_enum_with_context(val, "kind", node_type_str)?;
      let modifiers = get_required_node_vec_with_context(
        val,
        "modifiers",
        node_type_str,
        source_content,
      )?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let parameters = get_required_node_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;
      let return_parameters = get_required_node_with_context(
        val,
        "returnParameters",
        node_type_str,
        source_content,
      )?;
      let scope = get_required_i32_with_context(val, "scope", node_type_str)?;
      let state_mutability =
        get_required_enum_with_context(val, "stateMutability", node_type_str)?;
      let virtual_ =
        get_required_bool_with_context(val, "virtual", node_type_str)?;
      let visibility =
        get_required_enum_with_context(val, "visibility", node_type_str)?;

      Ok(ASTNode::FunctionDefinition {
        node_id,
        src_location,
        body,
        documentation,
        implemented,
        kind,
        modifiers,
        name,
        name_location,
        parameters,
        return_parameters,
        scope,
        state_mutability,
        virtual_,
        visibility,
      })
    }
    "EventDefinition" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let parameters = get_required_node_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::EventDefinition {
        node_id,
        src_location,
        name,
        name_location,
        parameters,
      })
    }
    "ErrorDefinition" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let parameters = get_required_node_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ErrorDefinition {
        node_id,
        src_location,
        name,
        name_location,
        parameters,
      })
    }
    "ModifierDefinition" => {
      let body = get_required_node_with_context(
        val,
        "body",
        node_type_str,
        source_content,
      )?;
      let documentation = get_optional_node_with_context(
        val,
        "documentation",
        node_type_str,
        source_content,
      )?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let parameters = get_required_node_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;
      let virtual_ =
        get_required_bool_with_context(val, "virtual", node_type_str)?;
      let visibility =
        get_required_enum_with_context(val, "visibility", node_type_str)?;

      Ok(ASTNode::ModifierDefinition {
        node_id,
        src_location,
        body,
        documentation,
        name,
        name_location,
        parameters,
        virtual_,
        visibility,
      })
    }
    "StructDefinition" => {
      let members = get_required_node_vec_with_context(
        val,
        "members",
        node_type_str,
        source_content,
      )?;
      let canonical_name =
        get_required_string_with_context(val, "canonicalName", node_type_str)?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;
      let visibility =
        get_required_enum_with_context(val, "visibility", node_type_str)?;

      Ok(ASTNode::StructDefinition {
        node_id,
        src_location,
        members,
        canonical_name,
        name,
        name_location,
        visibility,
      })
    }
    "EnumDefinition" => {
      let members = get_required_node_vec_with_context(
        val,
        "members",
        node_type_str,
        source_content,
      )?;
      let canonical_name =
        get_required_string_with_context(val, "canonicalName", node_type_str)?;
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let name_location = get_required_source_location_with_context(
        val,
        "nameLocation",
        node_type_str,
      )?;

      Ok(ASTNode::EnumDefinition {
        node_id,
        src_location,
        members,
        canonical_name,
        name,
        name_location,
      })
    }
    "UserDefinedValueTypeDefinition" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;
      let underlying_type = get_required_node_with_context(
        val,
        "underlyingType",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::UserDefinedValueTypeDefinition {
        node_id,
        src_location,
        name,
        underlying_type,
      })
    }
    "PragmaDirective" => {
      let literals =
        get_required_string_vec_with_context(val, "literals", node_type_str)?;

      Ok(ASTNode::PragmaDirective {
        node_id,
        src_location,
        literals,
      })
    }
    "ImportDirective" => {
      let absolute_path =
        get_required_string_with_context(val, "absolutePath", node_type_str)?;
      let file = get_required_string_with_context(val, "file", node_type_str)?;
      let source_unit =
        get_required_i32_with_context(val, "sourceUnit", node_type_str)?;

      Ok(ASTNode::ImportDirective {
        node_id,
        src_location,
        absolute_path,
        file,
        source_unit,
      })
    }
    "UsingForDirective" => {
      let global =
        get_required_bool_with_context(val, "global", node_type_str)?;
      let library_name = get_optional_node_with_context(
        val,
        "libraryName",
        node_type_str,
        source_content,
      )?;
      let type_name = get_optional_node_with_context(
        val,
        "typeName",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::UsingForDirective {
        node_id,
        src_location,
        global,
        library_name,
        type_name,
      })
    }
    "SourceUnit" => {
      let nodes = get_required_node_vec_with_context(
        val,
        "nodes",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::SourceUnit {
        node_id,
        src_location,
        nodes,
      })
    }
    "InheritanceSpecifier" => {
      let base_name = get_required_node_with_context(
        val,
        "baseName",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::InheritanceSpecifier {
        node_id,
        src_location,
        base_name,
      })
    }
    "ElementaryTypeName" => {
      let name = get_required_string_with_context(val, "name", node_type_str)?;

      Ok(ASTNode::ElementaryTypeName {
        node_id,
        src_location,
        name,
      })
    }
    "FunctionTypeName" => {
      let parameter_types = get_required_node_with_context(
        val,
        "parameterTypes",
        node_type_str,
        source_content,
      )?;
      let return_parameter_types = get_required_node_with_context(
        val,
        "returnParameterTypes",
        node_type_str,
        source_content,
      )?;
      let state_mutability =
        get_required_enum_with_context(val, "stateMutability", node_type_str)?;
      let visibility =
        get_required_enum_with_context(val, "visibility", node_type_str)?;

      Ok(ASTNode::FunctionTypeName {
        node_id,
        src_location,
        parameter_types,
        return_parameter_types,
        state_mutability,
        visibility,
      })
    }
    "ParameterList" => {
      let parameters = get_required_node_vec_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ParameterList {
        node_id,
        src_location,
        parameters,
      })
    }
    "TryCatchClause" => {
      let error_name =
        get_required_string_with_context(val, "errorName", node_type_str)?;
      let block = get_required_node_with_context(
        val,
        "block",
        node_type_str,
        source_content,
      )?;
      let parameters = get_optional_node_with_context(
        val,
        "parameters",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::TryCatchClause {
        node_id,
        src_location,
        error_name,
        block,
        parameters,
      })
    }
    "ModifierInvocation" => {
      let modifier_name = get_required_node_with_context(
        val,
        "modifierName",
        node_type_str,
        source_content,
      )?;
      let arguments = get_optional_node_vec_with_context(
        val,
        "arguments",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ModifierInvocation {
        node_id,
        src_location,
        modifier_name,
        arguments,
      })
    }
    "UserDefinedTypeName" => {
      let path_node = get_required_node_with_context(
        val,
        "pathNode",
        node_type_str,
        source_content,
      )?;
      let referenced_declaration = get_required_i32_with_context(
        val,
        "referencedDeclaration",
        node_type_str,
      )?;

      Ok(ASTNode::UserDefinedTypeName {
        node_id,
        src_location,
        path_node,
        referenced_declaration,
      })
    }
    "ArrayTypeName" => {
      let base_type = get_required_node_with_context(
        val,
        "baseType",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::ArrayTypeName {
        node_id,
        src_location,
        base_type,
      })
    }
    "Mapping" => {
      let key_name =
        get_optional_string_with_context(val, "keyName", node_type_str)?;
      let key_name_location = get_required_source_location_with_context(
        val,
        "keyNameLocation",
        node_type_str,
      )?;
      let key_type = get_required_node_with_context(
        val,
        "keyType",
        node_type_str,
        source_content,
      )?;
      let value_name =
        get_optional_string_with_context(val, "valueName", node_type_str)?;
      let value_name_location = get_required_source_location_with_context(
        val,
        "valueNameLocation",
        node_type_str,
      )?;
      let value_type = get_required_node_with_context(
        val,
        "valueType",
        node_type_str,
        source_content,
      )?;

      Ok(ASTNode::Mapping {
        node_id,
        src_location,
        key_name,
        key_name_location,
        key_type,
        value_name,
        value_name_location,
        value_type,
      })
    }
    "StructuredDocumentation" => {
      let text = get_required_string_with_context(val, "text", node_type_str)?;

      Ok(ASTNode::StructuredDocumentation {
        node_id,
        src_location,
        text,
      })
    }
    // Other node type
    _ => {
      let nodes = get_required_node_vec(val, "nodes", source_content)
        .map_err(|e| format!("Error parsing {} node: {}", node_type_str, e))
        .unwrap_or_else(|_| Vec::new());
      let body = get_optional_node(val, "body", source_content)
        .map_err(|e| format!("Error parsing {} node: {}", node_type_str, e))
        .unwrap_or(None);

      Ok(ASTNode::Other {
        node_id,
        src_location,
        nodes,
        body,
        node_type: node_type_str.to_string(),
      })
    }
  }
}
