pub use parser::{AST, ASTNode};
use std::collections::{BTreeMap, HashSet};
use std::path::Path;

pub mod parser;

pub fn analyze(
  project_root: &Path,
) -> Result<
  (
    BTreeMap<i32, FirstPassDeclaration>,
    HashSet<String>,
    BTreeMap<i32, InScopeDeclaration>,
  ),
  String,
> {
  // Load scope.txt file to determine which files are in audit scope (required)
  let in_scope_files = load_scope_file(project_root)?;

  // First pass: Parse all ASTs and build comprehensive declaration dictionary
  // This processes every declaration in every file, regardless of scope
  let ast_map = parser::process(project_root)?;
  let first_pass_declarations = first_pass(&ast_map, &in_scope_files)?;

  // Tree shaking: Build in-scope dictionary by following references from publicly visible declarations
  let in_scope_declarations = tree_shake(&first_pass_declarations)?;

  Ok((
    first_pass_declarations,
    in_scope_files,
    in_scope_declarations,
  ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub container: String,         // Source file path
  pub component: Option<String>, // Contract name
  pub member: Option<String>,    // Function name
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationKind {
  Contract(parser::ContractKind),
  Function(parser::FunctionKind),
  Modifier,
  Event,
  Error,
  Struct,
  Enum,
  EnumMember,
  Constant,
  StateVariable,
  LocalVariable,
}

/// First pass declaration structure used during initial AST traversal.
/// Contains basic declaration information and references without topic IDs.
/// This is used to build a comprehensive dictionary of all declarations
/// before determining which ones are in-scope and need detailed analysis.
/// The is_publicly_in_scope field indicates if the declaration is publicly
/// visible (contracts, public/external functions, constructors, fallback, receive).
///
/// Two variants exist:
/// - Block: For declarations that contain executable code (functions, modifiers)
///   These track referenced nodes and require/revert statements for analysis
/// - Flat: For simple declarations without executable code (contracts, structs, etc.)
///   These only track basic declaration information
#[derive(Debug, Clone)]
pub enum FirstPassDeclaration {
  Block {
    is_publicly_in_scope: bool,
    declaration_kind: DeclarationKind,
    referenced_nodes: Vec<i32>,
    require_revert_statements: Vec<i32>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<i32>,
  },
  Flat {
    is_publicly_in_scope: bool,
    declaration_kind: DeclarationKind,
  },
}

pub struct InScopeDeclaration {
  pub references: Vec<i32>,
  pub require_revert_statements: Vec<i32>,
  pub function_calls: Vec<i32>,
  pub variable_mutations: Vec<i32>,
  pub declaration_kind: DeclarationKind,
}

/// Second pass declaration structure for detailed analysis (TODO: to be implemented).
/// Will contain topic IDs and additional analysis data for in-scope declarations.
/// This will be populated after determining which declarations are actually needed
/// based on the in-scope analysis and reference following.
/// Only declarations that are publicly visible or referenced by publicly visible
/// declarations will be included in the second pass.
// TODO: Implement SecondPassDeclaration for detailed analysis
#[derive(Debug, Clone)]
pub struct SecondPassDeclaration {
  pub topic_id: i32,
  pub is_publicly_in_scope: bool,
  pub declaration_kind: DeclarationKind,
  pub name: String,
  pub scope: Scope,
  // For functions and modifiers
  pub referenced_nodes: Vec<i32>, // Node IDs of all referenced nodes
  pub require_revert_statements: Vec<i32>, // Node IDs of require/revert statements
                                  // Additional fields for detailed analysis will be added here
}

/// Analyzer implementing the two-pass architecture described in the README:
///
/// 1. First pass: Parse all ASTs and build comprehensive declaration dictionary
///    - Store all declarations with basic info and references
///    - Mark publicly visible declarations (contracts, public/external functions,
///      constructors, fallback, receive functions) in in-scope files
///    - All other declarations are marked as not publicly visible
///    - No topic ID assignment yet
///
/// 2. Second pass (TODO): Build in-scope dictionary with topic IDs
///    - Start from publicly visible in-scope declarations
///    - Follow references recursively to build complete in-scope set
///    - Assign sequential topic IDs to in-scope declarations
///    - Create detailed analysis data structures
/// Load scope.txt file from project root (required file).
/// Returns error if file doesn't exist or cannot be read.
/// Each line should contain a relative path to a file in audit scope.
fn load_scope_file(project_root: &Path) -> Result<HashSet<String>, String> {
  let scope_file = project_root.join("scope.txt");
  if !scope_file.exists() {
    return Err("scope.txt file not found in project root".to_string());
  }

  let content =
    std::fs::read_to_string(&scope_file).map_err(|e| format!("Failed to read scope.txt: {}", e))?;

  let mut in_scope_files = HashSet::new();
  for line in content.lines() {
    let line = line.trim();
    if !line.is_empty() {
      in_scope_files.insert(line.to_string());
    }
  }

  Ok(in_scope_files)
}

fn first_pass(
  ast_map: &std::collections::BTreeMap<String, Vec<AST>>,
  in_scope_files: &HashSet<String>,
) -> Result<BTreeMap<i32, FirstPassDeclaration>, String> {
  let mut first_pass_declarations = BTreeMap::new();

  for (path, asts) in ast_map {
    let is_file_in_scope = in_scope_files.contains(path);

    for ast in asts {
      process_ast_nodes(
        &ast.nodes(),
        ast.absolute_path(),
        is_file_in_scope,
        None,
        None,
        &mut first_pass_declarations,
      )?;
    }
  }

  Ok(first_pass_declarations)
}

fn process_ast_nodes(
  nodes: &[&ASTNode],
  file_path: &str,
  is_file_in_scope: bool,
  current_contract: Option<&str>,
  current_function: Option<&str>,
  first_pass_declarations: &mut BTreeMap<i32, FirstPassDeclaration>,
) -> Result<(), String> {
  for node in nodes {
    match node {
      ASTNode::ContractDefinition {
        node_id,
        name,
        contract_kind,
        ..
      } => {
        let declaration_kind = DeclarationKind::Contract(*contract_kind);

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_file_in_scope, // All contracts are publicly visible
            declaration_kind,
          },
        );

        let child_nodes = node.nodes();
        process_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          Some(name),
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::FunctionDefinition {
        node_id,
        name,
        kind,
        visibility,
        ..
      } => {
        let mut referenced_nodes = Vec::new();
        let mut require_revert_statements = Vec::new();
        let mut function_calls = Vec::new();
        let mut variable_mutations = Vec::new();

        // Process entire function node to find references and require/revert statements
        collect_references_and_statements(
          node,
          &mut referenced_nodes,
          &mut require_revert_statements,
          &mut function_calls,
          &mut variable_mutations,
        );

        // Determine if function is publicly visible
        let is_publicly_visible = if is_file_in_scope {
          match kind {
            parser::FunctionKind::Constructor
            | parser::FunctionKind::Fallback
            | parser::FunctionKind::Receive => true,
            parser::FunctionKind::Function | parser::FunctionKind::FreeFunction => {
              matches!(
                visibility,
                parser::FunctionVisibility::Public | parser::FunctionVisibility::External
              )
            }
          }
        } else {
          false
        };

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Block {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind: DeclarationKind::Function(*kind),
            referenced_nodes,
            require_revert_statements,
            function_calls,
            variable_mutations,
          },
        );

        // Process function body nodes for local variables
        let child_nodes = node.nodes();
        process_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          Some(name),
          first_pass_declarations,
        )?;
      }

      ASTNode::ModifierDefinition {
        node_id,
        visibility,
        ..
      } => {
        let mut referenced_nodes = Vec::new();
        let mut require_revert_statements = Vec::new();
        let mut function_calls = Vec::new();
        let mut variable_mutations = Vec::new();

        let is_publicly_visible = is_file_in_scope
          && matches!(
            visibility,
            parser::FunctionVisibility::Public | parser::FunctionVisibility::External
          );

        // Process entire modifier node to find references and require/revert statements
        collect_references_and_statements(
          node,
          &mut referenced_nodes,
          &mut require_revert_statements,
          &mut function_calls,
          &mut variable_mutations,
        );

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Block {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind: DeclarationKind::Modifier,
            referenced_nodes,
            require_revert_statements,
            function_calls,
            variable_mutations,
          },
        );

        // Process modifier body nodes
        let child_nodes = node.nodes();
        process_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::VariableDeclaration {
        node_id,
        visibility,
        state_variable,
        mutability,
        ..
      } => {
        let is_publicly_visible =
          is_file_in_scope && matches!(visibility, parser::VariableVisibility::Public);

        let declaration_kind = if *state_variable {
          if matches!(mutability, parser::VariableMutability::Constant) {
            DeclarationKind::Constant
          } else {
            DeclarationKind::StateVariable
          }
        } else {
          DeclarationKind::LocalVariable
        };

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind,
          },
        );
      }

      ASTNode::EventDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Event,
          },
        );
      }

      ASTNode::ErrorDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Error,
          },
        );
      }

      ASTNode::StructDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Struct,
          },
        );

        // Process struct members
        let member_nodes = node.nodes();
        process_ast_nodes(
          &member_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enums are not publicly visible
            declaration_kind: DeclarationKind::Enum,
          },
        );

        // Process enum members
        let member_nodes = node.nodes();
        process_ast_nodes(
          &member_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumValue { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enum members are not publicly visible
            declaration_kind: DeclarationKind::EnumMember,
          },
        );
      }

      _ => {
        // For other node types, recursively process their child nodes
        let child_nodes = node.nodes();
        process_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }
    }
  }

  Ok(())
}

fn collect_references_and_statements(
  node: &ASTNode,
  referenced_nodes: &mut Vec<i32>,
  require_revert_statements: &mut Vec<i32>,
  function_calls: &mut Vec<i32>,
  variable_mutations: &mut Vec<i32>,
) {
  match node {
    ASTNode::Identifier {
      referenced_declaration,
      ..
    } => {
      referenced_nodes.push(*referenced_declaration);
    }
    ASTNode::FunctionCall { expression, .. } => {
      // Check if this is a require or revert statement
      if let ASTNode::Identifier { name, .. } = expression.as_ref() {
        if name == "require" || name == "revert" {
          require_revert_statements.push(node.node_id());
        }
      }

      // Extract function call reference from the expression
      if let ASTNode::Identifier {
        referenced_declaration,
        ..
      } = expression.as_ref()
      {
        function_calls.push(*referenced_declaration);
      }

      // Continue traversing child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(
          child,
          referenced_nodes,
          require_revert_statements,
          function_calls,
          variable_mutations,
        );
      }
    }
    ASTNode::RevertStatement { node_id, .. } => {
      require_revert_statements.push(*node_id);

      // Continue traversing child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(
          child,
          referenced_nodes,
          require_revert_statements,
          function_calls,
          variable_mutations,
        );
      }
    }
    ASTNode::Assignment { left_hand_side, .. } => {
      // Check for variable mutations in assignments
      if let ASTNode::Identifier {
        referenced_declaration,
        ..
      } = left_hand_side.as_ref()
      {
        variable_mutations.push(*referenced_declaration);
      }

      // Continue traversing child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(
          child,
          referenced_nodes,
          require_revert_statements,
          function_calls,
          variable_mutations,
        );
      }
    }
    _ => {
      // For all other nodes, recursively traverse child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(
          child,
          referenced_nodes,
          require_revert_statements,
          function_calls,
          variable_mutations,
        );
      }
    }
  }
}

impl FirstPassDeclaration {
  /// Get the publicly in-scope status for any declaration variant
  pub fn is_publicly_in_scope(&self) -> bool {
    match self {
      FirstPassDeclaration::Block {
        is_publicly_in_scope,
        ..
      } => *is_publicly_in_scope,
      FirstPassDeclaration::Flat {
        is_publicly_in_scope,
        ..
      } => *is_publicly_in_scope,
    }
  }

  /// Get the declaration kind for any declaration variant
  pub fn declaration_kind(&self) -> &DeclarationKind {
    match self {
      FirstPassDeclaration::Block {
        declaration_kind, ..
      } => declaration_kind,
      FirstPassDeclaration::Flat {
        declaration_kind, ..
      } => declaration_kind,
    }
  }

  /// Get referenced nodes if this is a Block variant, otherwise empty slice
  pub fn referenced_nodes(&self) -> &[i32] {
    match self {
      FirstPassDeclaration::Block {
        referenced_nodes, ..
      } => referenced_nodes,
      FirstPassDeclaration::Flat { .. } => &[],
    }
  }

  /// Get require/revert statements if this is a Block variant, otherwise empty slice
  pub fn require_revert_statements(&self) -> &[i32] {
    match self {
      FirstPassDeclaration::Block {
        require_revert_statements,
        ..
      } => require_revert_statements,
      FirstPassDeclaration::Flat { .. } => &[],
    }
  }

  /// Get function calls if this is a Block variant, otherwise empty slice
  pub fn function_calls(&self) -> &[i32] {
    match self {
      FirstPassDeclaration::Block { function_calls, .. } => function_calls,
      FirstPassDeclaration::Flat { .. } => &[],
    }
  }

  /// Get variable mutations if this is a Block variant, otherwise empty slice
  pub fn variable_mutations(&self) -> &[i32] {
    match self {
      FirstPassDeclaration::Block {
        variable_mutations, ..
      } => variable_mutations,
      FirstPassDeclaration::Flat { .. } => &[],
    }
  }

  /// Check if this declaration has executable code (Block variant)
  pub fn has_executable_code(&self) -> bool {
    matches!(self, FirstPassDeclaration::Block { .. })
  }
}

/// Tree shake the first pass declarations to include only in-scope and used declarations.
/// Returns a map of node_id to InScopeDeclaration containing all nodes that reference each declaration.
fn tree_shake(
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
) -> Result<BTreeMap<i32, InScopeDeclaration>, String> {
  let mut in_scope_declarations = BTreeMap::new();
  let mut visited = HashSet::new();
  let mut visiting = HashSet::new(); // For cycle detection

  // First, collect all publicly in-scope declarations as starting points
  let publicly_visible: Vec<i32> = first_pass_declarations
    .iter()
    .filter(|(_, decl)| decl.is_publicly_in_scope())
    .map(|(node_id, _)| *node_id)
    .collect();

  // Process each publicly visible declaration recursively
  for &node_id in &publicly_visible {
    process_declaration_recursive(
      node_id,
      None, // No referencing node for root declarations
      first_pass_declarations,
      &mut in_scope_declarations,
      &mut visited,
      &mut visiting,
    )?;
  }

  Ok(in_scope_declarations)
}

/// Recursively process a declaration and all its references
fn process_declaration_recursive(
  node_id: i32,
  referencing_node: Option<i32>, // The node that references this declaration
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
  in_scope_declarations: &mut BTreeMap<i32, InScopeDeclaration>,
  visited: &mut HashSet<i32>,
  visiting: &mut HashSet<i32>,
) -> Result<(), String> {
  // Cycle detection
  if visiting.contains(&node_id) {
    // We found a cycle, but this is not necessarily an error in the code analysis
    // Just skip processing this node to avoid infinite recursion
    return Ok(());
  }

  // If already processed, add this reference and return
  if visited.contains(&node_id) {
    if let Some(in_scope_decl) = in_scope_declarations.get_mut(&node_id) {
      // Add the direct referencing node if it exists and isn't already in the list
      if let Some(ref_node) = referencing_node {
        if !in_scope_decl.references.contains(&ref_node) {
          in_scope_decl.references.push(ref_node);
        }
      }
    }
    return Ok(());
  }

  // Check if declaration exists in our first pass dictionary
  let first_pass_decl = match first_pass_declarations.get(&node_id) {
    Some(decl) => decl,
    None => {
      // Ignore references not found in our dictionary (external libraries, built-ins, etc.)
      return Ok(());
    }
  };

  // Mark as currently being visited (for cycle detection)
  visiting.insert(node_id);

  // Create new in-scope declaration with direct reference
  let mut references = Vec::new();
  if let Some(ref_node) = referencing_node {
    references.push(ref_node);
  }

  // Filter function calls to exclude events, errors, and modifiers
  let filtered_function_calls = first_pass_decl
    .function_calls()
    .iter()
    .filter(|&&call_id| {
      if let Some(called_decl) = first_pass_declarations.get(&call_id) {
        !matches!(
          called_decl.declaration_kind(),
          DeclarationKind::Event | DeclarationKind::Error | DeclarationKind::Modifier
        )
      } else {
        true // Keep calls to declarations not in our map (external references)
      }
    })
    .cloned()
    .collect();

  // Filter variable mutations to exclude local variables
  let filtered_variable_mutations = first_pass_decl
    .variable_mutations()
    .iter()
    .filter(|&&var_id| {
      if let Some(var_decl) = first_pass_declarations.get(&var_id) {
        !matches!(var_decl.declaration_kind(), DeclarationKind::LocalVariable)
      } else {
        true // Keep mutations of variables not in our map (external references)
      }
    })
    .cloned()
    .collect();

  in_scope_declarations.insert(
    node_id,
    InScopeDeclaration {
      references,
      require_revert_statements: first_pass_decl.require_revert_statements().to_vec(),
      function_calls: filtered_function_calls,
      variable_mutations: filtered_variable_mutations,
      declaration_kind: first_pass_decl.declaration_kind().clone(),
    },
  );

  // Process all referenced nodes from this declaration
  let referenced_nodes = first_pass_decl.referenced_nodes();

  for &referenced_node_id in referenced_nodes {
    process_declaration_recursive(
      referenced_node_id,
      Some(node_id), // This node is the one referencing the next node
      first_pass_declarations,
      in_scope_declarations,
      visited,
      visiting,
    )?;
  }

  // Mark as fully processed
  visiting.remove(&node_id);
  visited.insert(node_id);

  Ok(())
}
