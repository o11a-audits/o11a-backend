pub use parser::{AST, ASTNode};
use std::collections::{BTreeMap, HashSet};
use std::path::Path;

pub mod parser;

pub fn analyze(
  project_root: &Path,
) -> Result<(BTreeMap<i32, FirstPassDeclaration>, HashSet<String>), String> {
  // Load scope.txt file to determine which files are in audit scope (required)
  let in_scope_files = load_scope_file(project_root)?;

  // First pass: Parse all ASTs and build comprehensive declaration dictionary
  // This processes every declaration in every file, regardless of scope
  let ast_map = parser::process(project_root)?;
  let first_pass_declarations = first_pass(&ast_map, &in_scope_files)?;

  // Second pass: Build in-scope dictionary with topic IDs (TODO)
  // TODO: Implement second pass to:
  // 1. Start from publicly visible in-scope declarations (already identified)
  // 2. Follow references recursively to find all needed declarations
  // 3. Assign sequential topic IDs starting from 1
  // 4. Create SecondPassDeclaration instances for detailed analysis
  //
  // Public visibility rules applied in first pass:
  // - Contracts: Always publicly visible if in in-scope file
  // - Functions: Only if public/external visibility AND in in-scope file
  // - Constructors, Fallback, Receive: Always publicly visible if in in-scope file
  // - All other declarations (modifiers, events, errors, structs, enums,
  //   enum members, state variables, constants, local variables): Never publicly visible

  Ok((first_pass_declarations, in_scope_files))
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
    referenced_nodes: Vec<i32>,
    require_revert_statements: Vec<i32>,
  },
  Flat {
    is_publicly_in_scope: bool,
  },
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
      ASTNode::ContractDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_file_in_scope, // All contracts are publicly visible
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

        // Process entire function node to find references and require/revert statements
        collect_references_and_statements(
          node,
          &mut referenced_nodes,
          &mut require_revert_statements,
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
            referenced_nodes,
            require_revert_statements,
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
        );

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Block {
            is_publicly_in_scope: is_publicly_visible,
            referenced_nodes,
            require_revert_statements,
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
        ..
      } => {
        let is_publicly_visible =
          is_file_in_scope && matches!(visibility, parser::VariableVisibility::Public);

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_publicly_visible,
          },
        );
      }

      ASTNode::EventDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
          },
        );
      }

      ASTNode::ErrorDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
          },
        );
      }

      ASTNode::StructDefinition { node_id, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
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
) {
  match node {
    ASTNode::Identifier {
      referenced_declaration,
      ..
    } => {
      referenced_nodes.push(*referenced_declaration);
    }
    ASTNode::FunctionCall { expression, .. } => {
      // Check if this is a require statement
      if let ASTNode::Identifier { name, .. } = expression.as_ref() {
        if name == "require" {
          require_revert_statements.push(node.node_id());
        }
      }

      // Continue traversing child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(child, referenced_nodes, require_revert_statements);
      }
    }
    ASTNode::RevertStatement { node_id, .. } => {
      require_revert_statements.push(*node_id);

      // Continue traversing child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(child, referenced_nodes, require_revert_statements);
      }
    }
    _ => {
      // For all other nodes, recursively traverse child nodes
      let child_nodes = node.nodes();
      for child in child_nodes {
        collect_references_and_statements(child, referenced_nodes, require_revert_statements);
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
      } => *is_publicly_in_scope,
    }
  }

  /// Get the declaration kind for any declaration variant (not available in new structure)
  pub fn declaration_kind(&self) -> Option<&DeclarationKind> {
    None // Declaration kind is no longer stored in FirstPassDeclaration
  }

  /// Get the name for any declaration variant (not available in new structure)
  pub fn name(&self) -> Option<&str> {
    None // Name is no longer stored in FirstPassDeclaration
  }

  /// Get the scope for any declaration variant (not available in new structure)
  pub fn scope(&self) -> Option<&Scope> {
    None // Scope is no longer stored in FirstPassDeclaration
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

  /// Check if this declaration has executable code (Block variant)
  pub fn has_executable_code(&self) -> bool {
    matches!(self, FirstPassDeclaration::Block { .. })
  }
}
