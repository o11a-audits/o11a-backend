use crate::solidity::collaborator;
use crate::solidity::parser::{self, AST, ASTNode};
use std::collections::{BTreeMap, HashSet};
use std::path::Path;

pub struct DataContext {
  pub in_scope_files: HashSet<String>,
  pub nodes: BTreeMap<String, ASTNode>,
  pub declarations: BTreeMap<String, Declaration>,
  pub references: BTreeMap<String, Vec<String>>,
  pub function_properties: BTreeMap<String, FunctionModProperties>,
  pub source_content: BTreeMap<String, String>,
}

pub fn analyze(project_root: &Path) -> Result<DataContext, String> {
  // Load scope.txt file to determine which files are in audit scope (required)
  let in_scope_files = load_scope_file(project_root)?;

  // First pass: Parse all ASTs and build comprehensive declaration dictionary
  // This processes every declaration in every file, regardless of scope
  let ast_map = parser::process(project_root)?;
  let first_pass_declarations = first_pass(&ast_map, &in_scope_files)?;

  // Tree shaking: Build in-scope dictionary by following references from
  // publicly visible declarations
  let in_scope_declarations = tree_shake(&first_pass_declarations)?;

  // Second pass: Build final data structures for in-scope declarations
  let (nodes, declarations, references, function_properties, source_content) =
    second_pass(&ast_map, &in_scope_declarations)?;

  Ok(DataContext {
    in_scope_files,
    nodes,
    declarations,
    references,
    function_properties,
    source_content,
  })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub container: String,         // Source file path
  pub component: Option<String>, // Contract topic ID
  pub member: Option<String>,    // Function topic ID
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
  FunctionMod {
    is_publicly_in_scope: bool,
    declaration_kind: DeclarationKind,
    name: String,
    scope: Scope,
    referenced_nodes: Vec<i32>,
    require_revert_statements: Vec<i32>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<i32>,
  },
  Flat {
    is_publicly_in_scope: bool,
    declaration_kind: DeclarationKind,
    name: String,
    scope: Scope,
  },
}

pub enum InScopeDeclaration {
  // Functions and Modifiers
  FunctionMod {
    declaration_kind: DeclarationKind,
    name: String,
    scope: Scope,
    references: Vec<i32>,
    require_revert_statements: Vec<i32>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<i32>,
  },
  Flat {
    declaration_kind: DeclarationKind,
    name: String,
    scope: Scope,
    references: Vec<i32>,
  },
}

impl InScopeDeclaration {
  fn add_reference_if_not_present(&mut self, reference: i32) {
    match self {
      InScopeDeclaration::FunctionMod { references, .. }
      | InScopeDeclaration::Flat { references, .. } => {
        if !references.contains(&reference) {
          references.push(reference);
        }
      }
    }
  }

  pub fn declaration_kind(&self) -> &DeclarationKind {
    match self {
      InScopeDeclaration::FunctionMod {
        declaration_kind, ..
      }
      | InScopeDeclaration::Flat {
        declaration_kind, ..
      } => declaration_kind,
    }
  }

  pub fn name(&self) -> &String {
    match self {
      InScopeDeclaration::FunctionMod { name, .. }
      | InScopeDeclaration::Flat { name, .. } => name,
    }
  }

  pub fn scope(&self) -> &Scope {
    match self {
      InScopeDeclaration::FunctionMod { scope, .. }
      | InScopeDeclaration::Flat { scope, .. } => scope,
    }
  }

  /// Get the references for any declaration variant
  pub fn references(&self) -> &[i32] {
    match self {
      InScopeDeclaration::FunctionMod { references, .. } => references,
      InScopeDeclaration::Flat { references, .. } => references,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Declaration {
  pub topic_id: String,
  pub declaration_kind: DeclarationKind,
  pub name: String,
  pub scope: Scope,
}

pub enum FunctionModProperties {
  FunctionProperties {
    // Topic IDs of the local declarations of the function parameters
    parameters: Vec<String>,
    // Topic IDs of the declarations of the function return values
    returns: Vec<String>,
    // Topic IDs of the declarations of the function revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<String>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<String>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<String>,
  },
  ModifierProperties {
    // Topic IDs of the local declarations of the modifier parameters
    parameters: Vec<String>,
    // Topic IDs of the declarations of the modifier revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<String>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<String>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<String>,
  },
}

fn load_scope_file(project_root: &Path) -> Result<HashSet<String>, String> {
  let scope_file = project_root.join("scope.txt");
  if !scope_file.exists() {
    return Err("scope.txt file not found in project root".to_string());
  }

  let content = std::fs::read_to_string(&scope_file)
    .map_err(|e| format!("Failed to read scope.txt: {}", e))?;

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
      process_first_pass_ast_nodes(
        &ast.nodes.iter().collect(),
        &ast.absolute_path,
        is_file_in_scope,
        None,
        None,
        &mut first_pass_declarations,
      )?;
    }
  }

  Ok(first_pass_declarations)
}

fn process_first_pass_ast_nodes(
  nodes: &Vec<&ASTNode>,
  file_path: &String,
  is_file_in_scope: bool,
  current_contract: Option<&String>,
  current_function: Option<&String>,
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
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
            declaration_kind,
          },
        );

        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          Some(&collaborator::node_id_to_topic_id(*node_id)),
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
            parser::FunctionKind::Function
            | parser::FunctionKind::FreeFunction => {
              matches!(
                visibility,
                parser::FunctionVisibility::Public
                  | parser::FunctionVisibility::External
              )
            }
          }
        } else {
          false
        };

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::FunctionMod {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind: DeclarationKind::Function(*kind),
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
            referenced_nodes,
            require_revert_statements,
            function_calls,
            variable_mutations,
          },
        );

        // Process function body nodes for local variables
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          &file_path,
          is_file_in_scope,
          current_contract,
          Some(&collaborator::node_id_to_topic_id(*node_id)),
          first_pass_declarations,
        )?;
      }

      ASTNode::ModifierDefinition {
        node_id,
        visibility,
        name,
        ..
      } => {
        let mut referenced_nodes = Vec::new();
        let mut require_revert_statements = Vec::new();
        let mut function_calls = Vec::new();
        let mut variable_mutations = Vec::new();

        let is_publicly_visible = is_file_in_scope
          && matches!(
            visibility,
            parser::FunctionVisibility::Public
              | parser::FunctionVisibility::External
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
          FirstPassDeclaration::FunctionMod {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind: DeclarationKind::Modifier,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
            referenced_nodes,
            require_revert_statements,
            function_calls,
            variable_mutations,
          },
        );

        // Process modifier body nodes
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          Some(&collaborator::node_id_to_topic_id(*node_id)),
          first_pass_declarations,
        )?;
      }

      ASTNode::VariableDeclaration {
        node_id,
        visibility,
        state_variable,
        mutability,
        name,
        ..
      } => {
        let is_publicly_visible = is_file_in_scope
          && matches!(visibility, parser::VariableVisibility::Public);

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
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );
      }

      ASTNode::EventDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Event,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );
      }

      ASTNode::ErrorDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Error,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );
      }

      ASTNode::StructDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: DeclarationKind::Struct,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );

        // Process struct members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enums are not publicly visible
            declaration_kind: DeclarationKind::Enum,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );

        // Process enum members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          file_path,
          is_file_in_scope,
          current_contract,
          current_function,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumValue { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enum members are not publicly visible
            declaration_kind: DeclarationKind::EnumMember,
            name: name.clone(),
            scope: Scope {
              container: file_path.clone(),
              component: current_contract.map(|t| t.clone()),
              member: current_function.map(|t| t.clone()),
            },
          },
        );
      }

      _ => {
        // For other node types, recursively process their child nodes
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
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

/// Second pass: Parse each AST and build the final data structures for in-scope nodes
/// This processes each AST one at a time, checking declarations for inclusion in the
/// in-scope dictionary. When found, adds the node and all child nodes to the accumulating
/// data structures.
fn second_pass(
  ast_map: &BTreeMap<String, Vec<AST>>,
  in_scope_declarations: &BTreeMap<i32, InScopeDeclaration>,
) -> Result<
  (
    BTreeMap<String, ASTNode>,
    BTreeMap<String, Declaration>,
    BTreeMap<String, Vec<String>>,
    BTreeMap<String, FunctionModProperties>,
    BTreeMap<String, String>,
  ),
  String,
> {
  let mut nodes: BTreeMap<String, ASTNode> = BTreeMap::new();
  let mut declarations: BTreeMap<String, Declaration> = BTreeMap::new();
  let mut references: BTreeMap<String, Vec<String>> = BTreeMap::new();
  let mut function_properties: BTreeMap<String, FunctionModProperties> =
    BTreeMap::new();
  let mut source_content: BTreeMap<String, String> = BTreeMap::new();

  // Process each AST file
  for (file_path, asts) in ast_map {
    for ast in asts {
      let found_in_scope_node = process_second_pass_nodes(
        &ast.nodes.iter().collect(),
        file_path,
        None,  // No contract context initially
        None,  // No function context initially
        false, // Parent is not in scope - check each node
        in_scope_declarations,
        &mut nodes,
        &mut declarations,
        &mut references,
        &mut function_properties,
      )?;

      if found_in_scope_node == true {
        source_content.insert(file_path.clone(), ast.source_content.clone());
      }
    }
  }

  Ok((
    nodes,
    declarations,
    references,
    function_properties,
    source_content,
  ))
}

/// Recursively process nodes during the second pass
/// If parent_in_scope is true, all nodes are assumed to be in scope
fn process_second_pass_nodes(
  ast_nodes: &Vec<&ASTNode>,
  file_path: &str,
  current_contract: Option<&str>,
  current_function: Option<&str>,
  parent_in_scope: bool,
  in_scope_declarations: &BTreeMap<i32, InScopeDeclaration>,
  nodes: &mut BTreeMap<String, ASTNode>,
  declarations: &mut BTreeMap<String, Declaration>,
  references: &mut BTreeMap<String, Vec<String>>,
  function_properties: &mut BTreeMap<String, FunctionModProperties>,
) -> Result<bool, String> {
  let mut found_in_scope_node = false;

  for node in ast_nodes {
    let node_id = node.node_id();
    let topic_id = collaborator::node_id_to_topic_id(node_id);

    // Check if this node should be processed (either parent is in scope or it's in the in_scope_declarations)
    let in_scope_decl = in_scope_declarations.get(&node_id);
    let is_in_scope = parent_in_scope || in_scope_decl.is_some();

    if is_in_scope {
      // Add the node with its children converted to stubs
      let stubbed_node = parser::children_to_stubs((*node).clone());
      nodes.insert(topic_id.clone(), stubbed_node);
      // Mark that a node was found so we can return this to the caller
      found_in_scope_node = true;
    }

    // Process declarations only if they exist in in_scope_declarations
    if let Some(in_scope_decl) = in_scope_decl {
      // Add declaration
      declarations.insert(
        topic_id.clone(),
        Declaration {
          topic_id: collaborator::node_id_to_topic_id(node_id),
          declaration_kind: in_scope_decl.declaration_kind().clone(),
          name: in_scope_decl.name().clone(),
          scope: in_scope_decl.scope().clone(),
        },
      );

      // Store references to this declaration
      let ref_topic_ids: Vec<String> = in_scope_decl
        .references()
        .iter()
        .map(|&id| collaborator::node_id_to_topic_id(id))
        .collect();
      references.insert(topic_id.clone(), ref_topic_ids);

      match in_scope_decl {
        InScopeDeclaration::FunctionMod {
          require_revert_statements,
          function_calls,
          variable_mutations,
          ..
        } => {
          let revert_topic_ids: Vec<String> = require_revert_statements
            .iter()
            .map(|&id| collaborator::node_id_to_topic_id(id))
            .collect();
          let call_topic_ids: Vec<String> = function_calls
            .iter()
            .map(|&id| collaborator::node_id_to_topic_id(id))
            .collect();
          let mutation_topic_ids: Vec<String> = variable_mutations
            .iter()
            .map(|&id| collaborator::node_id_to_topic_id(id))
            .collect();

          match node {
            ASTNode::FunctionDefinition {
              parameters,
              return_parameters,
              ..
            } => {
              // Extract function properties
              let param_topic_ids = extract_parameter_topic_ids(parameters);
              let return_topic_ids =
                extract_parameter_topic_ids(return_parameters);

              function_properties.insert(
                topic_id.clone(),
                FunctionModProperties::FunctionProperties {
                  parameters: param_topic_ids,
                  returns: return_topic_ids,
                  reverts: revert_topic_ids,
                  calls: call_topic_ids,
                  mutations: mutation_topic_ids,
                },
              );
            }
            ASTNode::ModifierDefinition { parameters, .. } => {
              // Extract modifier properties
              let param_topic_ids = extract_parameter_topic_ids(parameters);

              function_properties.insert(
                topic_id.clone(),
                FunctionModProperties::ModifierProperties {
                  parameters: param_topic_ids,
                  reverts: revert_topic_ids,
                  calls: call_topic_ids,
                  mutations: mutation_topic_ids,
                },
              );
            }

            _ => (),
          }
        }
        _ => (),
      }
    }

    // Process children with appropriate context
    let child_nodes = node.nodes();
    if !child_nodes.is_empty() {
      // Update context based on node type
      let (new_contract, new_function) = match node {
        ASTNode::ContractDefinition { name, .. } => {
          (Some(name.as_str()), current_function)
        }
        ASTNode::FunctionDefinition { name, .. } => {
          (current_contract, Some(name.as_str()))
        }
        ASTNode::ModifierDefinition { .. } => {
          (current_contract, current_function)
        }
        _ => (current_contract, current_function),
      };

      process_second_pass_nodes(
        &child_nodes,
        file_path,
        new_contract,
        new_function,
        is_in_scope, // Children inherit parent's in-scope status
        in_scope_declarations,
        nodes,
        declarations,
        references,
        function_properties,
      )?;
    }
  }

  Ok(found_in_scope_node)
}

/// Extract parameter topic IDs from a ParameterList node
fn extract_parameter_topic_ids(parameter_list: &ASTNode) -> Vec<String> {
  let mut topic_ids = Vec::new();

  if let ASTNode::ParameterList { parameters, .. } = parameter_list {
    for param in parameters {
      topic_ids.push(collaborator::node_id_to_topic_id(param.node_id()));
    }
  }

  topic_ids
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
    ASTNode::FunctionCall {
      expression,
      arguments,
      ..
    } => {
      // Check if this is a require or revert function call
      if let ASTNode::Identifier {
        name,
        referenced_declaration,
        ..
      } = expression.as_ref()
      {
        if name == "require" {
          // For require, add the second argument (error message) if it exists
          if arguments.len() >= 2 {
            require_revert_statements.push(arguments[1].node_id());
          }
        } else if name == "revert" {
          // For revert function call, add the first argument if it exists
          if !arguments.is_empty() {
            require_revert_statements.push(arguments[0].node_id());
          }
        } else {
          // For other function calls, extract the function reference
          function_calls.push(*referenced_declaration);
        }
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
    ASTNode::RevertStatement { error_call, .. } => {
      // For RevertStatement, extract the error identifier from the error_call
      // The error_call is a FunctionCall node
      if let ASTNode::FunctionCall { expression, .. } = error_call.as_ref() {
        // The expression contains the error identifier
        if let ASTNode::Identifier {
          referenced_declaration,
          ..
        } = expression.as_ref()
        {
          require_revert_statements.push(*referenced_declaration);
        }
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
      FirstPassDeclaration::FunctionMod {
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
      FirstPassDeclaration::FunctionMod {
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
      FirstPassDeclaration::FunctionMod {
        referenced_nodes, ..
      } => referenced_nodes,
      FirstPassDeclaration::Flat { .. } => &[],
    }
  }
}

/// Tree shake the first pass declarations to include only in-scope and used declarations.
/// Returns a map of node_id to InScopeDeclaration containing all nodes that reference each declaration.
fn tree_shake(
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
) -> Result<BTreeMap<i32, InScopeDeclaration>, String> {
  let mut in_scope_declarations = BTreeMap::new();
  let mut visiting = HashSet::new(); // For cycle detection

  // First, collect all publicly in-scope declarations as starting points
  let publicly_visible: Vec<i32> = first_pass_declarations
    .iter()
    .filter(|(_, decl)| decl.is_publicly_in_scope())
    .map(|(node_id, _)| *node_id)
    .collect();

  // Process each publicly visible declaration recursively
  for &node_id in &publicly_visible {
    process_tree_shake_declarations(
      node_id,
      None, // No referencing node for root declarations
      first_pass_declarations,
      &mut in_scope_declarations,
      &mut visiting,
    )?;
  }

  Ok(in_scope_declarations)
}

/// Recursively process a declaration and all its references
fn process_tree_shake_declarations(
  node_id: i32,
  referencing_node: Option<i32>, // The node that references this declaration
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
  in_scope_declarations: &mut BTreeMap<i32, InScopeDeclaration>,
  visiting: &mut HashSet<i32>,
) -> Result<(), String> {
  // Cycle detection
  if visiting.contains(&node_id) {
    // We found a cycle, but this is not necessarily an error in the code analysis
    // Just skip processing this node to avoid infinite recursion
    return Ok(());
  }

  // If already processed, add this reference and return
  if let Some(in_scope_decl) = in_scope_declarations.get_mut(&node_id) {
    if let Some(ref_node) = referencing_node {
      in_scope_decl.add_reference_if_not_present(ref_node)
    }
    return Ok(());
  }

  // Check if declaration exists in our first pass dictionary
  let first_pass_decl = match first_pass_declarations.get(&node_id) {
    Some(decl) => decl,
    None => {
      // Ignore references not found in our dictionary (external libraries,
      // built-ins, etc.)
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

  let in_scope_decl = match first_pass_decl {
    FirstPassDeclaration::FunctionMod {
      declaration_kind,
      name,
      scope,
      require_revert_statements,
      function_calls,
      variable_mutations,
      ..
    } => {
      // Filter function calls to exclude events, errors, and modifiers
      let filtered_function_calls = function_calls
        .iter()
        .filter(|&&call_id| {
          if let Some(called_decl) = first_pass_declarations.get(&call_id) {
            !matches!(
              called_decl.declaration_kind(),
              DeclarationKind::Event
                | DeclarationKind::Error
                | DeclarationKind::Modifier
            )
          } else {
            // Keep calls to declarations not in our map (external references)
            true
          }
        })
        .cloned()
        .collect();

      // Filter variable mutations to exclude local variables
      let filtered_variable_mutations = variable_mutations
        .iter()
        .filter(|&&var_id| {
          if let Some(var_decl) = first_pass_declarations.get(&var_id) {
            !matches!(
              var_decl.declaration_kind(),
              DeclarationKind::LocalVariable
            )
          } else {
            // Keep mutations of variables not in our map (external references)
            true
          }
        })
        .cloned()
        .collect();

      InScopeDeclaration::FunctionMod {
        declaration_kind: declaration_kind.clone(),
        name: name.clone(),
        scope: scope.clone(),
        references: references,
        require_revert_statements: require_revert_statements.clone(),
        function_calls: filtered_function_calls,
        variable_mutations: filtered_variable_mutations,
      }
    }
    FirstPassDeclaration::Flat {
      declaration_kind,
      name,
      scope,
      ..
    } => InScopeDeclaration::Flat {
      declaration_kind: declaration_kind.clone(),
      name: name.clone(),
      scope: scope.clone(),
      references,
    },
  };

  in_scope_declarations.insert(node_id, in_scope_decl);

  // Process all referenced nodes from this declaration
  let referenced_nodes = first_pass_decl.referenced_nodes();

  for &referenced_node_id in referenced_nodes {
    process_tree_shake_declarations(
      referenced_node_id,
      Some(node_id), // This node is the one referencing the next node
      first_pass_declarations,
      in_scope_declarations,
      visiting,
    )?;
  }

  // Mark as fully processed
  visiting.remove(&node_id);

  Ok(())
}
