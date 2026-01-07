use crate::core;
use crate::core::topic;
use crate::core::{
  AST, DataContext, FunctionKind, FunctionModProperties, Node, Scope,
  TopicKind, TopicMetadata, VariableProperties,
};
use crate::solidity::parser::{self, ASTNode, SolidityAST};
use std::collections::{BTreeMap, HashSet};
use std::path::Path;

pub fn analyze(
  project_root: &Path,
  audit_id: &str,
  data_context: &mut DataContext,
) -> Result<(), String> {
  // Get or create the audit data
  let audit_data = data_context
    .get_audit_mut(audit_id)
    .ok_or_else(|| format!("Audit '{}' not found", audit_id))?;

  // First pass: Parse all ASTs and build comprehensive declaration dictionary
  // This processes every declaration in every file, regardless of scope
  let ast_map = parser::process(project_root)?;
  let first_pass_source_topics =
    first_pass(&ast_map, &audit_data.in_scope_files)?;

  // Tree shaking: Build in-scope dictionary by following references from
  // publicly visible declarations
  let in_scope_source_topics = tree_shake(&first_pass_source_topics)?;

  // Second pass: Build final data structures for in-scope declarations
  // Pass mutable references to audit_data's maps directly
  second_pass(
    &ast_map,
    &in_scope_source_topics,
    &mut audit_data.nodes,
    &mut audit_data.topic_metadata,
    &mut audit_data.references,
    &mut audit_data.function_properties,
    &mut audit_data.variable_properties,
    &mut audit_data.source_content,
  )?;

  // Insert ASTs with stubbed nodes
  for (path, ast_list) in ast_map {
    for ast in ast_list {
      let stubbed_ast = crate::solidity::parser::SolidityAST {
        node_id: ast.node_id,
        nodes: ast
          .nodes
          .iter()
          .map(|n| parser::children_to_stubs(n.clone()))
          .collect(),
        project_path: ast.project_path.clone(),
        source_content: ast.source_content.clone(),
      };
      audit_data
        .asts
        .insert(path.clone(), AST::Solidity(stubbed_ast));
    }
  }

  Ok(())
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
    declaration_kind: TopicKind,
    name: String,
    scope: Scope,
    referenced_nodes: Vec<i32>,
    require_revert_statements: Vec<i32>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<ReferencedNode>,
  },
  Flat {
    is_publicly_in_scope: bool,
    declaration_kind: TopicKind,
    name: String,
    scope: Scope,
  },
}

#[derive(Debug, Clone)]
pub struct ReferencedNode {
  statement_node: i32,
  referenced_node: i32,
}

pub enum InScopeDeclaration {
  // Functions and Modifiers
  FunctionMod {
    declaration_kind: TopicKind,
    name: String,
    scope: Scope,
    references: Vec<i32>,
    require_revert_statements: Vec<i32>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<ReferencedNode>,
  },
  // All other declarations
  Flat {
    declaration_kind: TopicKind,
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

  pub fn declaration_kind(&self) -> &TopicKind {
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

fn first_pass(
  ast_map: &std::collections::BTreeMap<core::ProjectPath, Vec<SolidityAST>>,
  in_scope_files: &HashSet<core::ProjectPath>,
) -> Result<BTreeMap<i32, FirstPassDeclaration>, String> {
  let mut first_pass_declarations = BTreeMap::new();

  for (path, asts) in ast_map {
    let is_file_in_scope = in_scope_files.contains(path);

    for ast in asts {
      process_first_pass_ast_nodes(
        &ast.nodes.iter().collect(),
        is_file_in_scope,
        &core::Scope::Container {
          container: path.clone(),
        },
        &mut first_pass_declarations,
      )?;
    }
  }

  Ok(first_pass_declarations)
}

fn process_first_pass_ast_nodes(
  nodes: &Vec<&ASTNode>,
  is_file_in_scope: bool,
  current_scope: &core::Scope,
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
        let declaration_kind = TopicKind::Contract(*contract_kind);

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_file_in_scope, // All contracts are publicly visible
            name: name.clone(),
            scope: current_scope.clone(),
            declaration_kind,
          },
        );

        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          &core::add_to_scope(current_scope, topic::new_node_topic(node_id)),
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
            FunctionKind::Constructor
            | FunctionKind::Fallback
            | FunctionKind::Receive => true,
            FunctionKind::Function | FunctionKind::FreeFunction => {
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
            declaration_kind: TopicKind::Function(*kind),
            name: name.clone(),
            scope: current_scope.clone(),
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
          is_file_in_scope,
          &core::add_to_scope(current_scope, topic::new_node_topic(node_id)),
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
            declaration_kind: TopicKind::Modifier,
            name: name.clone(),
            scope: current_scope.clone(),
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
          is_file_in_scope,
          &core::add_to_scope(current_scope, topic::new_node_topic(node_id)),
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
            TopicKind::Constant
          } else {
            TopicKind::StateVariable
          }
        } else {
          TopicKind::LocalVariable
        };

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: is_publicly_visible,
            declaration_kind,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );
      }

      ASTNode::EventDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: TopicKind::Event,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );
      }

      ASTNode::ErrorDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: TopicKind::Error,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );
      }

      ASTNode::StructDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false,
            declaration_kind: TopicKind::Struct,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );

        // Process struct members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          is_file_in_scope,
          &core::add_to_scope(&current_scope, topic::new_node_topic(node_id)),
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumDefinition { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enums are not publicly visible
            declaration_kind: TopicKind::Enum,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );

        // Process enum members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          is_file_in_scope,
          &core::add_to_scope(&current_scope, topic::new_node_topic(node_id)),
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumValue { node_id, name, .. } => {
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            is_publicly_in_scope: false, // Enum members are not publicly visible
            declaration_kind: TopicKind::EnumMember,
            name: name.clone(),
            scope: current_scope.clone(),
          },
        );
      }

      _ => {
        // For other node types, recursively process their child nodes
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          current_scope,
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
  ast_map: &BTreeMap<core::ProjectPath, Vec<SolidityAST>>,
  in_scope_source_topics: &BTreeMap<i32, InScopeDeclaration>,
  nodes: &mut BTreeMap<topic::Topic, Node>,
  topic_metadata: &mut BTreeMap<topic::Topic, TopicMetadata>,
  references: &mut BTreeMap<topic::Topic, Vec<topic::Topic>>,
  function_properties: &mut BTreeMap<topic::Topic, FunctionModProperties>,
  variable_properties: &mut BTreeMap<topic::Topic, VariableProperties>,
  source_content: &mut BTreeMap<core::ProjectPath, String>,
) -> Result<(), String> {
  // Process each AST file
  for (file_path, asts) in ast_map {
    for ast in asts {
      let found_in_scope_node = process_second_pass_nodes(
        &ast.nodes.iter().collect(),
        false, // Parent is not in scope automatically - check each node
        in_scope_source_topics,
        nodes,
        topic_metadata,
        references,
        function_properties,
        variable_properties,
      )?;

      if found_in_scope_node {
        source_content.insert(file_path.clone(), ast.source_content.clone());
      }
    }
  }

  Ok(())
}

/// Recursively process nodes during the second pass
/// If parent_in_scope is true, all nodes are assumed to be in scope
fn process_second_pass_nodes(
  ast_nodes: &Vec<&ASTNode>,
  parent_in_scope: bool,
  in_scope_source_topics: &BTreeMap<i32, InScopeDeclaration>,
  nodes: &mut BTreeMap<topic::Topic, Node>,
  topic_metadata: &mut BTreeMap<topic::Topic, TopicMetadata>,
  references: &mut BTreeMap<topic::Topic, Vec<topic::Topic>>,
  function_properties: &mut BTreeMap<topic::Topic, FunctionModProperties>,
  variable_properties: &mut BTreeMap<topic::Topic, VariableProperties>,
) -> Result<bool, String> {
  let mut found_in_scope_node = false;

  for node in ast_nodes {
    let node_id = node.node_id();
    let topic = topic::new_node_topic(&node_id);

    // Check if this node should be processed (either parent is in scope or it's in the in_scope_declarations)
    let in_scope_topic_metadata = in_scope_source_topics.get(&node_id);
    let is_in_scope = parent_in_scope || in_scope_topic_metadata.is_some();

    if is_in_scope {
      // Add the node with its children converted to stubs
      let stubbed_node =
        Node::Solidity(parser::children_to_stubs((*node).clone()));

      nodes.insert(topic.clone(), stubbed_node);
      // Mark that a node was found so we can return this to the caller
      found_in_scope_node = true;
    }

    // Process declarations only if they exist in in_scope_declarations
    if let Some(in_scope_topic_metadata) = in_scope_topic_metadata {
      // TODO! "statements that are not referenceable could be added to the dict here in the second pass once we know they are in scope"
      // Add declaration
      topic_metadata.insert(
        topic.clone(),
        TopicMetadata::NamedTopic {
          topic: topic::new_node_topic(&node_id),
          kind: in_scope_topic_metadata.declaration_kind().clone(),
          name: in_scope_topic_metadata.name().clone(),
          scope: in_scope_topic_metadata.scope().clone(),
        },
      );

      // Store references to this declaration
      let ref_topics: Vec<topic::Topic> = in_scope_topic_metadata
        .references()
        .iter()
        .map(|&id| topic::new_node_topic(&id))
        .collect();
      references.insert(topic.clone(), ref_topics);

      match in_scope_topic_metadata {
        InScopeDeclaration::FunctionMod {
          require_revert_statements,
          function_calls,
          variable_mutations,
          ..
        } => {
          let revert_topics: Vec<topic::Topic> = require_revert_statements
            .iter()
            .map(|&id| topic::new_node_topic(&id))
            .collect();
          let call_topics: Vec<topic::Topic> = function_calls
            .iter()
            .map(|&id| topic::new_node_topic(&id))
            .collect();
          let mutation_topics: Vec<topic::Topic> = variable_mutations
            .iter()
            .map(|ref_node| {
              // Add mutation references to the audit data
              let variable_topic =
                topic::new_node_topic(&ref_node.referenced_node);
              let statement_topic =
                topic::new_node_topic(&ref_node.statement_node);

              if let Some(properties) =
                variable_properties.get_mut(&variable_topic)
              {
                properties.mutations.push(statement_topic);
              } else {
                variable_properties.insert(
                  variable_topic,
                  VariableProperties {
                    mutations: vec![statement_topic],
                  },
                );
              }
              topic::new_node_topic(&ref_node.referenced_node)
            })
            .collect();

          match node {
            ASTNode::FunctionDefinition {
              parameters,
              return_parameters,
              ..
            } => {
              // Extract function properties
              let param_topics = extract_parameter_topics(parameters);
              let return_topics = extract_parameter_topics(return_parameters);

              function_properties.insert(
                topic.clone(),
                FunctionModProperties::FunctionProperties {
                  parameters: param_topics,
                  returns: return_topics,
                  reverts: revert_topics,
                  calls: call_topics,
                  mutations: mutation_topics,
                },
              );
            }
            ASTNode::ModifierDefinition { parameters, .. } => {
              // Extract modifier properties
              let param_topics = extract_parameter_topics(parameters);

              function_properties.insert(
                topic.clone(),
                FunctionModProperties::ModifierProperties {
                  parameters: param_topics,
                  reverts: revert_topics,
                  calls: call_topics,
                  mutations: mutation_topics,
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
      let child_found_in_scope = process_second_pass_nodes(
        &child_nodes,
        is_in_scope, // Children inherit parent's in-scope status
        in_scope_source_topics,
        nodes,
        topic_metadata,
        references,
        function_properties,
        variable_properties,
      )?;

      // Accumulate whether any in-scope nodes were found in children
      found_in_scope_node = found_in_scope_node || child_found_in_scope;
    }
  }

  Ok(found_in_scope_node)
}

/// Extract parameter topic IDs from a ParameterList node
fn extract_parameter_topics(parameter_list: &ASTNode) -> Vec<topic::Topic> {
  let mut topics = Vec::new();

  if let ASTNode::ParameterList { parameters, .. } = parameter_list {
    for param in parameters {
      topics.push(topic::new_node_topic(&param.node_id()));
    }
  }

  topics
}

fn collect_references_and_statements(
  node: &ASTNode,
  referenced_nodes: &mut Vec<i32>,
  require_revert_statements: &mut Vec<i32>,
  function_calls: &mut Vec<i32>,
  variable_mutations: &mut Vec<ReferencedNode>,
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
    ASTNode::Assignment {
      node_id,
      left_hand_side,
      ..
    } => {
      // Check for variable mutations in assignments
      if let ASTNode::Identifier {
        referenced_declaration,
        ..
      } = left_hand_side.as_ref()
      {
        variable_mutations.push(ReferencedNode {
          statement_node: *node_id,
          referenced_node: *referenced_declaration,
        });
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
  pub fn declaration_kind(&self) -> &TopicKind {
    match self {
      FirstPassDeclaration::FunctionMod {
        declaration_kind, ..
      } => declaration_kind,
      FirstPassDeclaration::Flat {
        declaration_kind, ..
      } => declaration_kind,
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
              TopicKind::Event | TopicKind::Error | TopicKind::Modifier
            )
          } else {
            // Keep calls to declarations not in our map (external references)
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
        variable_mutations: variable_mutations.clone(),
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

  // Process all referenced nodes from this declaration if it is a function
  // or modifier
  match first_pass_decl {
    FirstPassDeclaration::FunctionMod {
      referenced_nodes, ..
    } => {
      for &referenced_node_id in referenced_nodes {
        process_tree_shake_declarations(
          referenced_node_id,
          Some(node_id), // This node is the one referencing the next node
          first_pass_declarations,
          in_scope_declarations,
          visiting,
        )?;
      }
    }
    FirstPassDeclaration::Flat { .. } => (),
  };

  // Mark as fully processed
  visiting.remove(&node_id);

  Ok(())
}
