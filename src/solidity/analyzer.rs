use foundry_compilers_artifacts::Visibility;

use crate::core::topic;
use crate::core::{self, UnnamedTopicKind};
use crate::core::{
  AST, DataContext, ElementaryType, FunctionModProperties, NamedTopicKind,
  Node, RevertCondition, RevertConstraint, RevertConstraintKind, Scope,
  SolidityType, TopicMetadata,
};
use crate::solidity::parser::{
  self, ASTNode, FunctionVisibility, SolidityAST, VariableVisibility,
};
use crate::solidity::transform;
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

  // Parse all ASTs
  let mut ast_map = parser::process(project_root)?;

  // Transform phase: Apply AST transformations before the first pass
  // This wraps function call arguments with Argument nodes and remaps
  // interface member references to their implementation members in the AST.
  transform::transform_ast(&mut ast_map)?;

  // First pass: build a comprehensive declaration dictionary
  // This processes every declaration in every file, regardless of scope
  let first_pass_source_topics =
    first_pass(&ast_map, &audit_data.in_scope_files)?;

  // Ancestry pass: collect variable ancestry relationships
  let all_ancestors = ancestry_pass(&ast_map);

  // Tree shaking: Build in-scope dictionary by following references from
  // publicly visible declarations. Also builds a map of variable mutations.
  // Note: Interface references are already remapped to implementations in the AST
  // by the transform phase, so tree shaking naturally follows implementation references.
  let (in_scope_source_topics, mutations_map) =
    tree_shake(&first_pass_source_topics)?;

  // Filter ancestors to only in-scope variables and derive descendants
  let (ancestors_map, descendants_map) =
    filter_and_derive_descendants(&all_ancestors, &in_scope_source_topics);

  // Second pass: Build final data structures for in-scope declarations
  // Pass mutable references to audit_data's maps directly
  second_pass(
    &ast_map,
    &in_scope_source_topics,
    &mutations_map,
    &ancestors_map,
    &descendants_map,
    &mut audit_data.nodes,
    &mut audit_data.topic_metadata,
    &mut audit_data.function_properties,
    &mut audit_data.variable_types,
    &mut audit_data.variable_constraints,
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
      };
      audit_data
        .asts
        .insert(path.clone(), AST::Solidity(stubbed_ast));
    }
  }

  Ok(())
}

// ============================================================================
// First Pass Revert Constraint Types
// ============================================================================

/// Tracks the context of enclosing if statements during AST traversal.
/// Used to determine which conditions lead to a revert statement.
#[derive(Debug, Clone)]
pub struct IfContext {
  /// The node ID of the condition expression
  pub condition_node: i32,
  /// true if we're in the true branch, false if in else branch
  pub in_true_branch: bool,
  /// Variable node IDs referenced in this condition (collected during traversal)
  pub referenced_variable_nodes: Vec<i32>,
}

/// A single condition in the path to a revert (first pass, using node IDs).
#[derive(Debug, Clone)]
pub struct FirstPassRevertCondition {
  pub condition_node: i32,
  pub must_be_true: bool,
}

/// Represents a constraint from a require or revert statement (first pass).
#[derive(Debug, Clone)]
pub struct FirstPassRevertConstraint {
  pub statement_node: i32,
  /// Chain of if-conditions leading to this revert (outermost first)
  pub conditions: Vec<FirstPassRevertCondition>,
  pub kind: RevertConstraintKind,
  /// Node IDs of variables referenced in the condition expressions.
  /// Collected during first pass traversal when we have access to the full AST.
  pub referenced_variable_nodes: Vec<i32>,
}

// ============================================================================
// First Pass Declaration Types
// ============================================================================

/// First pass declaration structure used during initial AST traversal.
/// Contains basic declaration information and references without topic IDs.
/// This is used to build a comprehensive dictionary of all declarations
/// before determining which ones are in-scope and need detailed analysis.
/// The is_publicly_in_scope field indicates if the declaration is publicly
/// visible (contracts, public/external functions, constructors, fallback, receive).
///
/// Two variants exist:
/// - Block: For declarations that contain executable code (functions, modifiers)
///   These track referenced nodes and revert constraints for analysis
/// - Flat: For simple declarations without executable code (contracts, structs, etc.)
///   These only track basic declaration information
#[derive(Debug, Clone)]
pub enum FirstPassDeclaration {
  FunctionMod {
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
    referenced_nodes: Vec<ReferencedNode>,
    revert_constraints: Vec<FirstPassRevertConstraint>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<ReferencedNode>,
  },
  Contract {
    is_publicly_in_scope: bool,
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
    base_contracts: Vec<ReferencedNode>,
    other_contracts: Vec<ReferencedNode>,
    public_members: Vec<i32>,
    referenced_nodes: Vec<ReferencedNode>,
  },
  Flat {
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
  },
}

#[derive(Debug, Clone)]
pub struct ReferencedNode {
  statement_node: i32,
  referenced_node: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceProcessingMethod {
  Normal,
  ProcessAllContractMembers,
}

// ============================================================================
// Type Extraction
// ============================================================================

/// Extracts a SolidityType from a type AST node.
/// Returns None if the type cannot be determined.
pub fn extract_solidity_type(type_node: &ASTNode) -> Option<SolidityType> {
  match type_node {
    ASTNode::ElementaryTypeName { name, .. } => {
      parse_elementary_type_name(name).map(SolidityType::Elementary)
    }
    ASTNode::UserDefinedTypeName {
      referenced_declaration,
      ..
    } => Some(SolidityType::UserDefined {
      declaration_topic: topic::new_node_topic(referenced_declaration),
    }),
    ASTNode::ArrayTypeName { base_type, .. } => {
      let base = extract_solidity_type(base_type)?;
      // TODO: Extract array length from the AST if it's a fixed-size array
      Some(SolidityType::Array {
        base_type: Box::new(base),
        length: None,
      })
    }
    ASTNode::Mapping {
      key_type,
      value_type,
      ..
    } => {
      let key = extract_solidity_type(key_type)?;
      let value = extract_solidity_type(value_type)?;
      Some(SolidityType::Mapping {
        key_type: Box::new(key),
        value_type: Box::new(value),
      })
    }
    ASTNode::FunctionTypeName {
      parameter_types,
      return_parameter_types,
      ..
    } => {
      let params = extract_parameter_types(parameter_types);
      let returns = extract_parameter_types(return_parameter_types);
      Some(SolidityType::Function {
        parameter_types: params,
        return_types: returns,
      })
    }
    _ => None,
  }
}

/// Extracts types from a ParameterList node
fn extract_parameter_types(param_list: &ASTNode) -> Vec<SolidityType> {
  if let ASTNode::ParameterList { parameters, .. } = param_list {
    parameters
      .iter()
      .filter_map(|param| {
        if let ASTNode::VariableDeclaration { type_name, .. } = param {
          extract_solidity_type(type_name)
        } else {
          None
        }
      })
      .collect()
  } else {
    Vec::new()
  }
}

/// Parses an elementary type name string into an ElementaryType.
/// Handles: bool, address, address payable, string, bytes, bytesN, intN, uintN
fn parse_elementary_type_name(name: &str) -> Option<ElementaryType> {
  match name {
    "bool" => Some(ElementaryType::Bool),
    "address" => Some(ElementaryType::Address),
    "address payable" => Some(ElementaryType::AddressPayable),
    "string" => Some(ElementaryType::String),
    "bytes" => Some(ElementaryType::Bytes),
    _ => {
      // Try to parse bytesN (bytes1 to bytes32)
      if let Some(suffix) = name.strip_prefix("bytes") {
        if let Ok(n) = suffix.parse::<u8>() {
          if n >= 1 && n <= 32 {
            return Some(ElementaryType::FixedBytes(n));
          }
        }
      }
      // Try to parse uintN (uint8 to uint256)
      if let Some(suffix) = name.strip_prefix("uint") {
        if suffix.is_empty() {
          // "uint" defaults to uint256
          return Some(ElementaryType::Uint { bits: 256 });
        }
        if let Ok(bits) = suffix.parse::<u16>() {
          if bits >= 8 && bits <= 256 && bits % 8 == 0 {
            return Some(ElementaryType::Uint { bits });
          }
        }
      }
      // Try to parse intN (int8 to int256)
      if let Some(suffix) = name.strip_prefix("int") {
        if suffix.is_empty() {
          // "int" defaults to int256
          return Some(ElementaryType::Int { bits: 256 });
        }
        if let Ok(bits) = suffix.parse::<u16>() {
          if bits >= 8 && bits <= 256 && bits % 8 == 0 {
            return Some(ElementaryType::Int { bits });
          }
        }
      }
      None
    }
  }
}

pub enum InScopeDeclaration {
  // Functions and Modifiers
  FunctionMod {
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
    references: Vec<i32>,
    revert_constraints: Vec<FirstPassRevertConstraint>,
    function_calls: Vec<i32>,
    variable_mutations: Vec<ReferencedNode>,
  },
  Contract {
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
    references: Vec<i32>,
    base_contracts: Vec<ReferencedNode>,
    other_contracts: Vec<ReferencedNode>,
    public_members: Vec<i32>,
  },
  // All other declarations
  Flat {
    declaration_kind: NamedTopicKind,
    visibility: Visibility,
    name: String,
    references: Vec<i32>,
  },
}

impl InScopeDeclaration {
  pub fn add_reference_if_not_present(&mut self, reference: i32) {
    match self {
      InScopeDeclaration::FunctionMod { references, .. }
      | InScopeDeclaration::Flat { references, .. }
      | InScopeDeclaration::Contract { references, .. } => {
        if !references.contains(&reference) {
          references.push(reference);
        }
      }
    }
  }

  pub fn declaration_kind(&self) -> &NamedTopicKind {
    match self {
      InScopeDeclaration::FunctionMod {
        declaration_kind, ..
      }
      | InScopeDeclaration::Flat {
        declaration_kind, ..
      }
      | InScopeDeclaration::Contract {
        declaration_kind, ..
      } => declaration_kind,
    }
  }

  pub fn name(&self) -> &String {
    match self {
      InScopeDeclaration::FunctionMod { name, .. }
      | InScopeDeclaration::Contract { name, .. }
      | InScopeDeclaration::Flat { name, .. } => name,
    }
  }

  /// Get the references for any declaration variant
  pub fn references(&self) -> &[i32] {
    match self {
      InScopeDeclaration::FunctionMod { references, .. }
      | InScopeDeclaration::Contract { references, .. }
      | InScopeDeclaration::Flat { references, .. } => references,
    }
  }

  pub fn visibility(&self) -> &Visibility {
    match self {
      InScopeDeclaration::FunctionMod { visibility, .. }
      | InScopeDeclaration::Contract { visibility, .. }
      | InScopeDeclaration::Flat { visibility, .. } => visibility,
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
        &mut first_pass_declarations,
      )?;
    }
  }

  Ok(first_pass_declarations)
}

/// Ancestry pass: Collects variable ancestry relationships from the AST.
/// This traverses the AST to find assignments, initializations, function arguments,
/// and return statements to determine which variables flow into which other variables.
fn ancestry_pass(
  ast_map: &std::collections::BTreeMap<core::ProjectPath, Vec<SolidityAST>>,
) -> AncestorsMap {
  let mut ancestors_map = AncestorsMap::new();

  for (_path, asts) in ast_map {
    for ast in asts {
      for node in &ast.nodes {
        collect_ancestry_from_node(node, None, &mut ancestors_map);
      }
    }
  }

  ancestors_map
}

fn process_first_pass_ast_nodes(
  nodes: &Vec<&ASTNode>,
  is_file_in_scope: bool,
  first_pass_declarations: &mut BTreeMap<i32, FirstPassDeclaration>,
) -> Result<(), String> {
  for node in nodes {
    match node {
      ASTNode::ContractDefinition {
        node_id,
        signature,
        nodes: contract_nodes,
        ..
      } => {
        // Extract fields from the ContractSignature
        let (
          signature_node_id,
          name,
          contract_kind,
          base_contracts,
          directives,
        ) = match signature.as_ref() {
          ASTNode::ContractSignature {
            node_id,
            name,
            contract_kind,
            base_contracts,
            directives,
            ..
          } => (node_id, name, contract_kind, base_contracts, directives),
          _ => {
            panic!("Expected ContractSignature in ContractDefinition.signature")
          }
        };

        let declaration_kind = NamedTopicKind::Contract(*contract_kind);

        // When getting these base contract node ids, set the signature node as
        // the containing statement node
        let base_contract_ids: Vec<ReferencedNode> = base_contracts
          .iter()
          .map(|base_contract| {
            // Each base_contract should be an InheritanceSpecifier
            match base_contract {
              ASTNode::InheritanceSpecifier { base_name, .. } => {
                // base_name should be an IdentifierPath with a referenced_declaration
                match base_name.as_ref() {
                  ASTNode::IdentifierPath { referenced_declaration, .. } => ReferencedNode {
                    statement_node: *signature_node_id,
                    referenced_node: *referenced_declaration,
                  },
                  _ => panic!(
                    "Expected IdentifierPath in InheritanceSpecifier base_name, got: {:?}",
                    base_name
                  ),
                }
              }
              _ => panic!(
                "Expected InheritanceSpecifier in base_contracts, got: {:?}",
                base_contract
              ),
            }
          })
          .collect();

        // When getting these directive node ids, set the signature node as
        // the containing statement node
        let using_for_contracts: Vec<ReferencedNode> = directives
          .iter()
          .filter_map(|node| match node {
            ASTNode::UsingForDirective {
              library_name,
              type_name,
              ..
            } => {
              // Helper function to extract referenced_declaration from either IdentifierPath or UserDefinedTypeName
              let extract_reference = |node: &ASTNode| match node {
                ASTNode::IdentifierPath {
                  referenced_declaration,
                  ..
                } => Some(ReferencedNode {
                  statement_node: *signature_node_id,
                  referenced_node: *referenced_declaration,
                }),
                ASTNode::UserDefinedTypeName { path_node, .. } => {
                  match path_node.as_ref() {
                    ASTNode::IdentifierPath {
                      referenced_declaration,
                      ..
                    } => Some(ReferencedNode {
                      statement_node: *signature_node_id,
                      referenced_node: *referenced_declaration,
                    }),
                    _ => None,
                  }
                }
                _ => None,
              };

              // Extract referenced_declaration from library_name
              let library_ref = library_name
                .as_ref()
                .and_then(|lib_node| extract_reference(lib_node.as_ref()));

              // Extract referenced_declaration from type_name
              let type_ref = type_name
                .as_ref()
                .and_then(|type_node| extract_reference(type_node.as_ref()));

              // Return both references if they exist
              match (library_ref, type_ref) {
                (Some(lib), Some(typ)) => Some(vec![lib, typ]),
                (Some(lib), None) => Some(vec![lib]),
                (None, Some(typ)) => Some(vec![typ]),
                (None, None) => None,
              }
            }
            _ => None,
          })
          .flatten()
          .collect();

        let public_member_ids: Vec<i32> = contract_nodes
          .iter()
          .filter_map(|node| match node {
            // Public functions
            ASTNode::FunctionDefinition {
              node_id, signature, ..
            } => {
              if let ASTNode::FunctionSignature { visibility, .. } =
                signature.as_ref()
              {
                if matches!(
                  visibility,
                  FunctionVisibility::Public | FunctionVisibility::External
                ) {
                  Some(*node_id)
                } else {
                  None
                }
              } else {
                None
              }
            }
            // Public modifiers
            ASTNode::ModifierDefinition {
              node_id, signature, ..
            } => {
              let visibility = match signature.as_ref() {
                ASTNode::ModifierSignature { visibility, .. } => visibility,
                _ => panic!("Expected ModifierSignature"),
              };
              if matches!(
                visibility,
                FunctionVisibility::Public | FunctionVisibility::External
              ) {
                Some(*node_id)
              } else {
                None
              }
            }
            // Events (all events are public)
            ASTNode::EventDefinition { node_id, .. } => Some(*node_id),
            // Errors (all errors are public)
            ASTNode::ErrorDefinition { node_id, .. } => Some(*node_id),
            ASTNode::StructDefinition {
              node_id,
              visibility,
              ..
            } if *visibility == VariableVisibility::Public => Some(*node_id),
            ASTNode::EnumDefinition { node_id, .. } => Some(*node_id),
            // Public state variables
            ASTNode::VariableDeclaration {
              node_id,
              visibility,
              state_variable,
              ..
            } if *state_variable
              && matches!(visibility, VariableVisibility::Public) =>
            {
              Some(*node_id)
            }
            _ => None,
          })
          .collect();

        // Collect type references from state variable declarations
        let mut variable_type_references: Vec<ReferencedNode> = Vec::new();
        for contract_node in contract_nodes {
          match contract_node {
            ASTNode::VariableDeclaration {
              node_id: var_node_id,
              state_variable,
              type_name,
              ..
            } if *state_variable => {
              collect_type_references(
                type_name,
                *var_node_id,
                &mut variable_type_references,
              );
            }
            ASTNode::StructDefinition {
              node_id, members, ..
            } => {
              for node in members {
                collect_type_references(
                  node,
                  *node_id,
                  &mut variable_type_references,
                );
              }
            }
            _ => (),
          }
        }

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Contract {
            is_publicly_in_scope: is_file_in_scope, // All contracts are publicly visible
            name: name.clone(),
            declaration_kind,
            visibility: Visibility::Public,
            base_contracts: base_contract_ids,
            other_contracts: using_for_contracts,
            public_members: public_member_ids,
            referenced_nodes: variable_type_references,
          },
        );

        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::FunctionDefinition {
        node_id, signature, ..
      } => {
        // Extract name, kind, and visibility from the signature
        let (name, kind, visibility) = match signature.as_ref() {
          ASTNode::FunctionSignature {
            name,
            kind,
            visibility,
            ..
          } => (name, kind, visibility),
          _ => {
            panic!("Expected FunctionSignature in FunctionDefinition.signature")
          }
        };

        let mut referenced_nodes = Vec::new();
        let mut revert_constraints = Vec::new();
        let mut function_calls = Vec::new();
        let mut variable_mutations = Vec::new();

        // Process entire function node to find references and revert constraints
        collect_references_and_statements(
          node,
          None, // No semantic block context initially
          &[],  // No enclosing if statements initially
          &mut referenced_nodes,
          &mut revert_constraints,
          &mut function_calls,
          &mut variable_mutations,
        );

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::FunctionMod {
            declaration_kind: NamedTopicKind::Function(*kind),
            visibility: function_visibility_to_visibility(visibility),
            name: name.clone(),
            referenced_nodes,
            revert_constraints,
            function_calls,
            variable_mutations,
          },
        );

        // Process function body nodes for local variables
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::ModifierDefinition {
        node_id, signature, ..
      } => {
        let name = match signature.as_ref() {
          ASTNode::ModifierSignature { name, .. } => name,
          _ => panic!("Expected ModifierSignature"),
        };

        let mut referenced_nodes = Vec::new();
        let mut revert_constraints = Vec::new();
        let mut function_calls = Vec::new();
        let mut variable_mutations = Vec::new();

        // Process entire modifier node to find references and revert constraints
        collect_references_and_statements(
          node,
          None, // No semantic block context initially
          &[],  // No enclosing if statements initially
          &mut referenced_nodes,
          &mut revert_constraints,
          &mut function_calls,
          &mut variable_mutations,
        );

        // Modifiers are always internal visibility
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::FunctionMod {
            declaration_kind: NamedTopicKind::Modifier,
            visibility: Visibility::Internal,
            name: name.clone(),
            referenced_nodes,
            revert_constraints,
            function_calls,
            variable_mutations,
          },
        );

        // Process modifier body nodes
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::VariableDeclaration {
        node_id,
        state_variable,
        mutability,
        name,
        visibility,
        ..
      } => {
        let declaration_kind = if *state_variable {
          NamedTopicKind::StateVariable(mutability.clone())
        } else {
          NamedTopicKind::LocalVariable
        };

        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind,
            visibility: variable_visibility_to_visibility(visibility),
            name: name.clone(),
          },
        );
      }

      ASTNode::EventDefinition { node_id, name, .. } => {
        // Events don't have visibility in Solidity, but are effectively public
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind: NamedTopicKind::Event,
            visibility: Visibility::Public,
            name: name.clone(),
          },
        );

        // Process event parameters
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::ErrorDefinition { node_id, name, .. } => {
        // Errors don't have visibility in Solidity, but are effectively public
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind: NamedTopicKind::Error,
            visibility: Visibility::Public,
            name: name.clone(),
          },
        );

        // Process error parameters
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::StructDefinition {
        node_id,
        name,
        visibility,
        ..
      } => {
        // Structs don't have visibility in Solidity, but are effectively public
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind: NamedTopicKind::Struct,
            visibility: variable_visibility_to_visibility(visibility),
            name: name.clone(),
          },
        );

        // Process struct members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumDefinition { node_id, name, .. } => {
        // Enums don't have visibility in Solidity, but are effectively public
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind: NamedTopicKind::Enum,
            visibility: Visibility::Public,
            name: name.clone(),
          },
        );

        // Process enum members
        let member_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &member_nodes,
          is_file_in_scope,
          first_pass_declarations,
        )?;
      }

      ASTNode::EnumValue { node_id, name, .. } => {
        // Enum values don't have visibility in Solidity, but are effectively public
        first_pass_declarations.insert(
          *node_id,
          FirstPassDeclaration::Flat {
            declaration_kind: NamedTopicKind::EnumMember,
            visibility: Visibility::Public,
            name: name.clone(),
          },
        );
      }

      _ => {
        // For other node types, recursively process their child nodes
        let child_nodes = node.nodes();
        process_first_pass_ast_nodes(
          &child_nodes,
          is_file_in_scope,
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
  mutations_map: &BTreeMap<i32, Vec<i32>>,
  ancestors_map: &AncestorsMap,
  descendants_map: &DescendantsMap,
  nodes: &mut BTreeMap<topic::Topic, Node>,
  topic_metadata: &mut BTreeMap<topic::Topic, TopicMetadata>,
  function_properties: &mut BTreeMap<topic::Topic, FunctionModProperties>,
  variable_types: &mut BTreeMap<topic::Topic, SolidityType>,
  variable_constraints: &mut BTreeMap<topic::Topic, Vec<topic::Topic>>,
) -> Result<(), String> {
  // Process each AST file
  for (file_path, asts) in ast_map {
    for ast in asts {
      process_second_pass_nodes(
        &ast.nodes.iter().collect(),
        false, // Parent is not in scope automatically - check each node
        in_scope_source_topics,
        mutations_map,
        ancestors_map,
        descendants_map,
        nodes,
        &core::Scope::Container {
          container: file_path.clone(),
        },
        topic_metadata,
        function_properties,
        variable_types,
        variable_constraints,
      )?;
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
  mutations_map: &BTreeMap<i32, Vec<i32>>,
  ancestors_map: &AncestorsMap,
  descendants_map: &DescendantsMap,
  nodes: &mut BTreeMap<topic::Topic, Node>,
  scope: &Scope,
  topic_metadata: &mut BTreeMap<topic::Topic, TopicMetadata>,
  function_properties: &mut BTreeMap<topic::Topic, FunctionModProperties>,
  variable_types: &mut BTreeMap<topic::Topic, SolidityType>,
  variable_constraints: &mut BTreeMap<topic::Topic, Vec<topic::Topic>>,
) -> Result<(), String> {
  for node in ast_nodes {
    let node_id = node.node_id();
    let topic = topic::new_node_topic(&node_id);

    // Check if this node should be processed (either parent is in scope or it's in the in_scope_declarations)
    let in_scope_topic_declaration = in_scope_source_topics.get(&node_id);
    let is_in_scope = parent_in_scope || in_scope_topic_declaration.is_some();

    if is_in_scope {
      // Add the node with its children converted to stubs
      let stubbed_node =
        Node::Solidity(parser::children_to_stubs((*node).clone()));

      nodes.insert(topic.clone(), stubbed_node);
    }

    // Process declarations only if they exist in in_scope_declarations
    if let Some(in_scope_topic_declaration) = in_scope_topic_declaration {
      // Build references to this declaration
      let ref_topics: Vec<topic::Topic> = in_scope_topic_declaration
        .references()
        .iter()
        .map(|&id| topic::new_node_topic(&id))
        .collect();

      // Build ancestor and descendant topics for this declaration
      let ancestor_topics: Vec<topic::Topic> = ancestors_map
        .get(&node_id)
        .map(|ids| ids.iter().map(|&id| topic::new_node_topic(&id)).collect())
        .unwrap_or_default();

      let descendant_topics: Vec<topic::Topic> = descendants_map
        .get(&node_id)
        .map(|ids| ids.iter().map(|&id| topic::new_node_topic(&id)).collect())
        .unwrap_or_default();

      // Check if this declaration has mutations (making it a NamedMutableTopic)
      let topic_metadata_entry = if let Some(mutation_node_ids) =
        mutations_map.get(&node_id)
        && (*in_scope_topic_declaration.declaration_kind()
          == NamedTopicKind::StateVariable(core::VariableMutability::Mutable)
          || *in_scope_topic_declaration.declaration_kind()
            == NamedTopicKind::LocalVariable)
      {
        let mutation_topics: Vec<topic::Topic> = mutation_node_ids
          .iter()
          .map(|&id| topic::new_node_topic(&id))
          .collect();
        TopicMetadata::NamedMutableTopic {
          topic: topic.clone(),
          kind: match in_scope_topic_declaration.declaration_kind() {
            NamedTopicKind::StateVariable(..) => {
              core::NamedMutableTopicKind::StateVariable
            }
            NamedTopicKind::LocalVariable => {
              core::NamedMutableTopicKind::LocalVariable
            }
            _ => unreachable!(
              "A declaration with mutations can only be a variable"
            ),
          },
          visibility: visibility_to_variable_visibility(
            in_scope_topic_declaration.visibility(),
          ),
          name: in_scope_topic_declaration.name().clone(),
          scope: scope.clone(),
          references: ref_topics,
          mutations: mutation_topics,
          ancestors: ancestor_topics,
          descendants: descendant_topics,
        }
      } else {
        TopicMetadata::NamedTopic {
          topic: topic.clone(),
          kind: in_scope_topic_declaration.declaration_kind().clone(),
          visibility: visibility_to_named_topic_visibility(
            in_scope_topic_declaration.visibility(),
          ),
          name: in_scope_topic_declaration.name().clone(),
          scope: scope.clone(),
          references: ref_topics,
          ancestors: ancestor_topics,
          descendants: descendant_topics,
        }
      };

      topic_metadata.insert(topic.clone(), topic_metadata_entry);

      match in_scope_topic_declaration {
        InScopeDeclaration::FunctionMod {
          revert_constraints: first_pass_constraints,
          function_calls,
          variable_mutations,
          ..
        } => {
          // Convert first-pass constraints to final RevertConstraint objects
          // and collect variable references for the variable_constraints map
          let final_revert_constraints: Vec<RevertConstraint> =
            first_pass_constraints
              .iter()
              .map(|fp_constraint| {
                let statement_topic =
                  topic::new_node_topic(&fp_constraint.statement_node);

                // Convert conditions from node IDs to topics
                let conditions: Vec<RevertCondition> = fp_constraint
                  .conditions
                  .iter()
                  .map(|cond| RevertCondition {
                    condition_topic: topic::new_node_topic(
                      &cond.condition_node,
                    ),
                    must_be_true: cond.must_be_true,
                  })
                  .collect();

                // Convert pre-collected reference node IDs to topics.
                // Filter to only include in-scope variable declarations
                // (excluding constants, enums, structs, etc.)
                let referenced_variables: Vec<topic::Topic> = fp_constraint
                  .referenced_variable_nodes
                  .iter()
                  .filter_map(|&node_id| {
                    if let Some(decl) = in_scope_source_topics.get(&node_id) {
                      if matches!(
                        decl.declaration_kind(),
                        NamedTopicKind::StateVariable(_)
                          | NamedTopicKind::LocalVariable
                      ) {
                        return Some(topic::new_node_topic(&node_id));
                      }
                    }
                    None
                  })
                  .collect();

                // Index this constraint by each referenced variable
                for var_topic in &referenced_variables {
                  variable_constraints
                    .entry(var_topic.clone())
                    .or_insert_with(Vec::new)
                    .push(statement_topic.clone());
                }

                RevertConstraint {
                  statement_topic,
                  conditions,
                  kind: fp_constraint.kind,
                  referenced_variables,
                }
              })
              .collect();

          // Keep the simple revert topics list for backwards compatibility
          let revert_topics: Vec<topic::Topic> = first_pass_constraints
            .iter()
            .map(|c| topic::new_node_topic(&c.statement_node))
            .collect();

          let call_topics: Vec<topic::Topic> = function_calls
            .iter()
            .map(|&id| topic::new_node_topic(&id))
            .collect();
          let mutation_topics: Vec<topic::Topic> = variable_mutations
            .iter()
            .map(|ref_node| topic::new_node_topic(&ref_node.referenced_node))
            .collect();

          match node {
            ASTNode::FunctionDefinition { .. } => {
              function_properties.insert(
                topic.clone(),
                FunctionModProperties::FunctionProperties {
                  reverts: revert_topics,
                  calls: call_topics,
                  mutations: mutation_topics,
                  revert_constraints: final_revert_constraints,
                },
              );
            }
            ASTNode::ModifierDefinition { .. } => {
              function_properties.insert(
                topic.clone(),
                FunctionModProperties::ModifierProperties {
                  reverts: revert_topics,
                  calls: call_topics,
                  mutations: mutation_topics,
                  revert_constraints: final_revert_constraints,
                },
              );
            }

            _ => (),
          }
        }

        _ => (),
      }

      // Extract variable types for VariableDeclaration nodes
      if let ASTNode::VariableDeclaration { type_name, .. } = node {
        if let Some(solidity_type) = extract_solidity_type(type_name) {
          variable_types.insert(topic.clone(), solidity_type);
        }
      }
    } else {
      let kind = match node {
        ASTNode::Assignment { .. } => UnnamedTopicKind::VariableMutation,
        ASTNode::BinaryOperation { operator, .. } => match operator {
          parser::BinaryOperator::Add => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Subtract => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Multiply => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Divide => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Modulo => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Power => UnnamedTopicKind::Arithmetic,
          parser::BinaryOperator::Equal => UnnamedTopicKind::Comparison,
          parser::BinaryOperator::NotEqual => UnnamedTopicKind::Comparison,
          parser::BinaryOperator::LessThan => UnnamedTopicKind::Comparison,
          parser::BinaryOperator::LessThanOrEqual => {
            UnnamedTopicKind::Comparison
          }
          parser::BinaryOperator::GreaterThan => UnnamedTopicKind::Comparison,
          parser::BinaryOperator::GreaterThanOrEqual => {
            UnnamedTopicKind::Comparison
          }
          parser::BinaryOperator::And => UnnamedTopicKind::Logical,
          parser::BinaryOperator::Or => UnnamedTopicKind::Logical,
          parser::BinaryOperator::BitwiseAnd => UnnamedTopicKind::Bitwise,
          parser::BinaryOperator::BitwiseOr => UnnamedTopicKind::Bitwise,
          parser::BinaryOperator::BitwiseXor => UnnamedTopicKind::Bitwise,
          parser::BinaryOperator::LeftShift => UnnamedTopicKind::Bitwise,
          parser::BinaryOperator::RightShift => UnnamedTopicKind::Bitwise,
        },
        ASTNode::Conditional { .. } => UnnamedTopicKind::Conditional,
        ASTNode::FunctionCall { .. } => UnnamedTopicKind::FunctionCall,
        ASTNode::TypeConversion { .. } => UnnamedTopicKind::TypeConversion,
        ASTNode::StructConstructor { .. } => {
          UnnamedTopicKind::StructConstruction
        }
        ASTNode::NewExpression { .. } => UnnamedTopicKind::NewExpression,
        ASTNode::SemanticBlock { .. } => UnnamedTopicKind::SemanticBlock,
        ASTNode::Break { .. } => UnnamedTopicKind::Break,
        ASTNode::Continue { .. } => UnnamedTopicKind::Continue,
        ASTNode::DoWhileStatement { .. } => UnnamedTopicKind::DoWhile,
        ASTNode::EmitStatement { .. } => UnnamedTopicKind::Emit,
        ASTNode::ForStatement { .. } => UnnamedTopicKind::For,
        ASTNode::IfStatement { .. } => UnnamedTopicKind::If,
        ASTNode::InlineAssembly { .. } => UnnamedTopicKind::InlineAssembly,
        ASTNode::PlaceholderStatement { .. } => UnnamedTopicKind::Placeholder,
        ASTNode::Return { .. } => UnnamedTopicKind::Return,
        ASTNode::RevertStatement { .. } => UnnamedTopicKind::Revert,
        ASTNode::TryStatement { .. } => UnnamedTopicKind::Try,
        ASTNode::UncheckedBlock { .. } => UnnamedTopicKind::UncheckedBlock,
        ASTNode::WhileStatement { .. } => UnnamedTopicKind::While,
        ASTNode::MemberAccess {
          referenced_declaration: None,
          ..
        } => UnnamedTopicKind::Reference,
        ASTNode::Identifier {
          referenced_declaration,
          ..
        }
        | ASTNode::IdentifierPath {
          referenced_declaration,
          ..
        }
        | ASTNode::MemberAccess {
          referenced_declaration: Some(referenced_declaration),
          ..
        } => {
          // Check if the referenced variable has any mutations in scope
          if mutations_map.contains_key(referenced_declaration) {
            UnnamedTopicKind::MutableReference
          } else {
            UnnamedTopicKind::Reference
          }
        }
        ASTNode::FunctionSignature { .. }
        | ASTNode::ModifierSignature { .. }
        | ASTNode::ContractSignature { .. } => UnnamedTopicKind::Signature,
        _ => UnnamedTopicKind::Other,
      };

      topic_metadata.insert(
        topic.clone(),
        TopicMetadata::UnnamedTopic {
          topic,
          scope: scope.clone(),
          kind,
        },
      );
    }

    // Process children with appropriate context
    let child_nodes = node.nodes();
    if !child_nodes.is_empty() {
      let scope = match node {
        // Do NOT add signature nodes to scope, it makes traversal annoying.
        // Signature nodes can still be set to the containing statement
        // of a reference, and can be rendered alongside the param definitions.
        ASTNode::SemanticBlock { node_id, .. }
        | ASTNode::ContractDefinition { node_id, .. }
        | ASTNode::FunctionDefinition { node_id, .. }
        | ASTNode::ModifierDefinition { node_id, .. }
        | ASTNode::StructDefinition { node_id, .. }
        | ASTNode::EnumDefinition { node_id, .. }
        | ASTNode::EventDefinition { node_id, .. }
        | ASTNode::ErrorDefinition { node_id, .. } => {
          &core::add_to_scope(&scope, topic::new_node_topic(node_id))
        }
        _ => scope,
      };

      process_second_pass_nodes(
        &child_nodes,
        is_in_scope, // Children inherit parent's in-scope status
        in_scope_source_topics,
        mutations_map,
        ancestors_map,
        descendants_map,
        nodes,
        scope,
        topic_metadata,
        function_properties,
        variable_types,
        variable_constraints,
      )?;
    }
  }

  Ok(())
}

/// Collects all reference node IDs from an expression during first pass.
/// This recursively traverses the expression tree to find all Identifier/IdentifierPath
/// nodes. The actual filtering to variables happens during the second pass when we
/// have the in-scope declarations.
fn collect_potential_variable_refs_from_expression(
  node: &ASTNode,
  refs: &mut Vec<i32>,
) {
  match node {
    ASTNode::Identifier {
      referenced_declaration,
      ..
    }
    | ASTNode::IdentifierPath {
      referenced_declaration,
      ..
    } => {
      if !refs.contains(referenced_declaration) {
        refs.push(*referenced_declaration);
      }
    }
    _ => {}
  }

  // Recursively process all children
  for child in node.nodes() {
    collect_potential_variable_refs_from_expression(child, refs);
  }
}

fn collect_references_and_statements(
  node: &ASTNode,
  current_semantic_block: Option<i32>,
  enclosing_ifs: &[IfContext],
  referenced_nodes: &mut Vec<ReferencedNode>,
  revert_constraints: &mut Vec<FirstPassRevertConstraint>,
  function_calls: &mut Vec<i32>,
  variable_mutations: &mut Vec<ReferencedNode>,
) {
  // Update current_semantic_block when entering a SemanticBlock
  let semantic_block = match node {
    ASTNode::SemanticBlock { node_id, .. } => Some(*node_id),
    ASTNode::FunctionSignature { node_id, .. } => Some(*node_id),
    _ => current_semantic_block,
  };

  match node {
    // References
    ASTNode::Identifier {
      referenced_declaration,
      ..
    }
    | ASTNode::IdentifierPath {
      referenced_declaration,
      ..
    } => {
      if let Some(block_id) = semantic_block {
        referenced_nodes.push(ReferencedNode {
          statement_node: block_id,
          referenced_node: *referenced_declaration,
        });
      }
    }

    // Member access (e.g., EnumType.Value, contract.member)
    ASTNode::MemberAccess {
      referenced_declaration: Some(referenced_declaration),
      ..
    } => {
      if let Some(block_id) = semantic_block {
        referenced_nodes.push(ReferencedNode {
          statement_node: block_id,
          referenced_node: *referenced_declaration,
        });
      }
    }

    // Function calls - check for require() which creates a constraint
    ASTNode::FunctionCall {
      node_id,
      expression,
      arguments,
      referenced_return_declarations,
      ..
    } => {
      // Add references to the function's return parameter declarations
      // This links the function call to the return variable nodes, enabling
      // ancestry traversal through the return values
      if let Some(block_id) = semantic_block {
        for &return_decl_id in referenced_return_declarations {
          referenced_nodes.push(ReferencedNode {
            statement_node: block_id,
            referenced_node: return_decl_id,
          });
        }
      }

      if let ASTNode::Identifier {
        name,
        referenced_declaration,
        ..
      } = expression.as_ref()
      {
        if name == "require" {
          // require(condition) reverts when condition is false
          // Create a constraint with the condition and must_be_true=false
          if !arguments.is_empty() {
            let condition_node = arguments[0].node_id();

            // Collect variable references from require's condition
            let mut referenced_variable_nodes: Vec<i32> = Vec::new();
            collect_potential_variable_refs_from_expression(
              &arguments[0],
              &mut referenced_variable_nodes,
            );

            // Also collect from enclosing if conditions
            for ctx in enclosing_ifs {
              for var_ref in &ctx.referenced_variable_nodes {
                if !referenced_variable_nodes.contains(var_ref) {
                  referenced_variable_nodes.push(*var_ref);
                }
              }
            }

            let mut conditions: Vec<FirstPassRevertCondition> = enclosing_ifs
              .iter()
              .map(|ctx| FirstPassRevertCondition {
                condition_node: ctx.condition_node,
                must_be_true: ctx.in_true_branch,
              })
              .collect();
            // Add the require condition itself (reverts when false)
            conditions.push(FirstPassRevertCondition {
              condition_node,
              must_be_true: false,
            });
            revert_constraints.push(FirstPassRevertConstraint {
              statement_node: *node_id,
              conditions,
              kind: RevertConstraintKind::Require,
              referenced_variable_nodes,
            });
          }
        } else if name == "revert" {
          // revert() as a function call (with optional error)
          // Capture all enclosing if conditions
          let conditions: Vec<FirstPassRevertCondition> = enclosing_ifs
            .iter()
            .map(|ctx| FirstPassRevertCondition {
              condition_node: ctx.condition_node,
              must_be_true: ctx.in_true_branch,
            })
            .collect();

          // Collect variable references from enclosing if conditions
          let mut referenced_variable_nodes: Vec<i32> = Vec::new();
          for ctx in enclosing_ifs {
            for var_ref in &ctx.referenced_variable_nodes {
              if !referenced_variable_nodes.contains(var_ref) {
                referenced_variable_nodes.push(*var_ref);
              }
            }
          }

          revert_constraints.push(FirstPassRevertConstraint {
            statement_node: *node_id,
            conditions,
            kind: RevertConstraintKind::Revert,
            referenced_variable_nodes,
          });
        } else {
          // For other function calls, extract the function reference
          function_calls.push(*referenced_declaration);
        }
      }
    }

    // RevertStatement - capture all enclosing if conditions
    ASTNode::RevertStatement { node_id, .. } => {
      let conditions: Vec<FirstPassRevertCondition> = enclosing_ifs
        .iter()
        .map(|ctx| FirstPassRevertCondition {
          condition_node: ctx.condition_node,
          must_be_true: ctx.in_true_branch,
        })
        .collect();

      // Collect variable references from enclosing if conditions
      let mut referenced_variable_nodes: Vec<i32> = Vec::new();
      for ctx in enclosing_ifs {
        for var_ref in &ctx.referenced_variable_nodes {
          if !referenced_variable_nodes.contains(var_ref) {
            referenced_variable_nodes.push(*var_ref);
          }
        }
      }

      revert_constraints.push(FirstPassRevertConstraint {
        statement_node: *node_id,
        conditions,
        kind: RevertConstraintKind::Revert,
        referenced_variable_nodes,
      });
    }

    // IfStatement - handle true and false branches with different contexts
    ASTNode::IfStatement {
      condition,
      true_body,
      false_body,
      ..
    } => {
      // First, collect references from the condition itself
      collect_references_and_statements(
        condition,
        semantic_block,
        enclosing_ifs,
        referenced_nodes,
        revert_constraints,
        function_calls,
        variable_mutations,
      );

      // Collect variable references from the condition expression
      let mut condition_var_refs: Vec<i32> = Vec::new();
      collect_potential_variable_refs_from_expression(
        condition,
        &mut condition_var_refs,
      );

      // Process true branch with condition context (must_be_true = true)
      let mut true_branch_ifs = enclosing_ifs.to_vec();
      true_branch_ifs.push(IfContext {
        condition_node: condition.node_id(),
        in_true_branch: true,
        referenced_variable_nodes: condition_var_refs.clone(),
      });
      collect_references_and_statements(
        true_body,
        semantic_block,
        &true_branch_ifs,
        referenced_nodes,
        revert_constraints,
        function_calls,
        variable_mutations,
      );

      // Process false branch (else) with condition context (must_be_true = false)
      if let Some(false_branch) = false_body {
        let mut false_branch_ifs = enclosing_ifs.to_vec();
        false_branch_ifs.push(IfContext {
          condition_node: condition.node_id(),
          in_true_branch: false,
          referenced_variable_nodes: condition_var_refs,
        });
        collect_references_and_statements(
          false_branch,
          semantic_block,
          &false_branch_ifs,
          referenced_nodes,
          revert_constraints,
          function_calls,
          variable_mutations,
        );
      }

      // Don't process children again - we handled them above
      return;
    }

    // Mutations - Assignments (including compound assignments like +=, -=, etc.)
    ASTNode::Assignment {
      node_id,
      left_hand_side,
      ..
    } => {
      // Extract the base variable being mutated from the left hand side
      if let Some(referenced_declaration) =
        extract_base_variable_reference(left_hand_side)
      {
        variable_mutations.push(ReferencedNode {
          statement_node: *node_id,
          referenced_node: referenced_declaration,
        });
      }
    }

    // Mutations - Unary operations (++, --, delete)
    ASTNode::UnaryOperation {
      node_id,
      operator,
      sub_expression,
      ..
    } => {
      // Only increment, decrement, and delete are mutating operators
      if matches!(
        operator,
        parser::UnaryOperator::Increment
          | parser::UnaryOperator::Decrement
          | parser::UnaryOperator::Delete
      ) {
        if let Some(referenced_declaration) =
          extract_base_variable_reference(sub_expression)
        {
          variable_mutations.push(ReferencedNode {
            statement_node: *node_id,
            referenced_node: referenced_declaration,
          });
        }
      }
    }

    _ => (),
  }

  // Continue traversing child nodes
  let child_nodes = node.nodes();
  for child in child_nodes {
    collect_references_and_statements(
      child,
      semantic_block,
      enclosing_ifs,
      referenced_nodes,
      revert_constraints,
      function_calls,
      variable_mutations,
    );
  }
}

/// Extracts the base variable's referenced_declaration from an expression that
/// may be mutated. Handles direct variable references (Identifier, IdentifierPath),
/// member access chains (e.g., someStruct.field), and index access chains
/// (e.g., arr[i], mapping[key]).
///
/// Returns the referenced_declaration of the base variable, or None if it cannot
/// be determined (e.g., for complex expressions or when the reference is missing).
fn extract_base_variable_reference(node: &ASTNode) -> Option<i32> {
  match node {
    // Direct variable reference
    ASTNode::Identifier {
      referenced_declaration,
      ..
    } => Some(*referenced_declaration),

    // Path-based variable reference (e.g., SomeContract.someVar)
    ASTNode::IdentifierPath {
      referenced_declaration,
      ..
    } => Some(*referenced_declaration),

    // Member access (e.g., someStruct.field) - recurse to find the base variable
    ASTNode::MemberAccess { expression, .. } => {
      extract_base_variable_reference(expression)
    }

    // Index access (e.g., arr[i], mapping[key]) - recurse to find the base variable
    ASTNode::IndexAccess {
      base_expression, ..
    } => extract_base_variable_reference(base_expression),

    // For other node types (e.g., function calls returning values), we can't
    // determine a base variable
    _ => None,
  }
}

/// Recursively extracts type references from a type AST node.
fn collect_type_references(
  type_node: &ASTNode,
  statement_node: i32,
  references: &mut Vec<ReferencedNode>,
) {
  match type_node {
    ASTNode::VariableDeclaration { type_name, .. } => {
      collect_type_references(type_name, statement_node, references);
    }
    ASTNode::UserDefinedTypeName {
      referenced_declaration,
      ..
    } => {
      references.push(ReferencedNode {
        statement_node,
        referenced_node: *referenced_declaration,
      });
    }
    ASTNode::Mapping {
      key_type,
      value_type,
      ..
    } => {
      collect_type_references(key_type, statement_node, references);
      collect_type_references(value_type, statement_node, references);
    }
    ASTNode::ArrayTypeName { base_type, .. } => {
      collect_type_references(base_type, statement_node, references);
    }
    _ => (),
  }
}

impl FirstPassDeclaration {
  /// Get the declaration kind for any declaration variant
  pub fn declaration_kind(&self) -> &NamedTopicKind {
    match self {
      FirstPassDeclaration::FunctionMod {
        declaration_kind, ..
      } => declaration_kind,
      FirstPassDeclaration::Contract {
        declaration_kind, ..
      } => declaration_kind,
      FirstPassDeclaration::Flat {
        declaration_kind, ..
      } => declaration_kind,
    }
  }
}

/// Convert parser::FunctionVisibility to foundry_compilers_artifacts::Visibility
fn function_visibility_to_visibility(
  vis: &parser::FunctionVisibility,
) -> Visibility {
  match vis {
    parser::FunctionVisibility::Public => Visibility::Public,
    parser::FunctionVisibility::Private => Visibility::Private,
    parser::FunctionVisibility::Internal => Visibility::Internal,
    parser::FunctionVisibility::External => Visibility::External,
  }
}

/// Convert parser::VariableVisibility to foundry_compilers_artifacts::Visibility
fn variable_visibility_to_visibility(
  vis: &parser::VariableVisibility,
) -> Visibility {
  match vis {
    parser::VariableVisibility::Public => Visibility::Public,
    parser::VariableVisibility::Private => Visibility::Private,
    parser::VariableVisibility::Internal => Visibility::Internal,
  }
}

/// Convert foundry_compilers_artifacts::Visibility to core::NamedTopicVisibility
fn visibility_to_named_topic_visibility(
  vis: &Visibility,
) -> core::NamedTopicVisibility {
  match vis {
    Visibility::Public => core::NamedTopicVisibility::Public,
    Visibility::Private => core::NamedTopicVisibility::Private,
    Visibility::Internal => core::NamedTopicVisibility::Internal,
    Visibility::External => core::NamedTopicVisibility::External,
  }
}

/// Convert foundry_compilers_artifacts::Visibility to parser::VariableVisibility
fn visibility_to_variable_visibility(
  vis: &Visibility,
) -> parser::VariableVisibility {
  match vis {
    Visibility::Public => parser::VariableVisibility::Public,
    Visibility::Private => parser::VariableVisibility::Private,
    Visibility::Internal => parser::VariableVisibility::Internal,
    // External is not valid for variables, treat as internal
    Visibility::External => parser::VariableVisibility::Internal,
  }
}

/// Tree shake the first pass declarations to include only in-scope and used declarations.
/// Returns a tuple of:
/// - A map of node_id to InScopeDeclaration containing all nodes that reference each declaration
/// - A map of variable node_id to Vec of mutation node_ids (assignment/unary operation nodes)
fn tree_shake(
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
) -> Result<(BTreeMap<i32, InScopeDeclaration>, BTreeMap<i32, Vec<i32>>), String>
{
  let mut in_scope_declarations = BTreeMap::new();
  let mut mutations_map: BTreeMap<i32, Vec<i32>> = BTreeMap::new();
  let mut visiting = HashSet::new(); // For cycle detection

  // First, collect all publicly in-scope declarations as starting points
  let in_scope_contracts: Vec<i32> = first_pass_declarations
    .iter()
    .filter_map(|(node_id, decl)| match decl {
      FirstPassDeclaration::Contract {
        is_publicly_in_scope: true,
        ..
      } => Some(*node_id),
      _ => None,
    })
    .collect();

  // Process each publicly visible declaration recursively
  for &node_id in &in_scope_contracts {
    process_tree_shake_declarations(
      node_id,
      None, // No referencing node for root declarations
      ReferenceProcessingMethod::ProcessAllContractMembers, // Process all public members of in scope contracts
      first_pass_declarations,
      &mut in_scope_declarations,
      &mut mutations_map,
      &mut visiting,
    )?;
  }

  Ok((in_scope_declarations, mutations_map))
}

/// Recursively process a declaration and all its references
fn process_tree_shake_declarations(
  node_id: i32,
  referencing_node: Option<i32>, // The node that references this declaration
  reference_processing_method: ReferenceProcessingMethod,
  first_pass_declarations: &BTreeMap<i32, FirstPassDeclaration>,
  in_scope_declarations: &mut BTreeMap<i32, InScopeDeclaration>,
  mutations_map: &mut BTreeMap<i32, Vec<i32>>,
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
  let in_scope_decl = match first_pass_decl {
    FirstPassDeclaration::FunctionMod {
      declaration_kind,
      visibility,
      name,
      revert_constraints,
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
              NamedTopicKind::Event
                | NamedTopicKind::Error
                | NamedTopicKind::Modifier
            )
          } else {
            // Keep calls to declarations not in our map (external references)
            true
          }
        })
        .cloned()
        .collect();

      // Collect mutations into the mutations_map
      // Maps variable_node_id -> Vec<mutation_node_id>
      for mutation in variable_mutations {
        mutations_map
          .entry(mutation.referenced_node)
          .or_insert_with(Vec::new)
          .push(mutation.statement_node);
      }

      let references = referencing_node.into_iter().collect();
      InScopeDeclaration::FunctionMod {
        declaration_kind: declaration_kind.clone(),
        visibility: visibility.clone(),
        name: name.clone(),
        references,
        revert_constraints: revert_constraints.clone(),
        function_calls: filtered_function_calls,
        variable_mutations: variable_mutations.clone(),
      }
    }
    FirstPassDeclaration::Flat {
      declaration_kind,
      visibility,
      name,
      ..
    } => {
      let references = referencing_node.into_iter().collect();
      InScopeDeclaration::Flat {
        declaration_kind: declaration_kind.clone(),
        visibility: visibility.clone(),
        name: name.clone(),
        references,
      }
    }
    FirstPassDeclaration::Contract {
      declaration_kind,
      visibility,
      name,
      base_contracts,
      other_contracts,
      public_members,
      ..
    } => {
      let references = referencing_node.into_iter().collect();
      InScopeDeclaration::Contract {
        declaration_kind: declaration_kind.clone(),
        visibility: visibility.clone(),
        name: name.clone(),
        references,
        base_contracts: base_contracts.clone(),
        other_contracts: other_contracts.clone(),
        public_members: public_members.clone(),
      }
    }
  };

  in_scope_declarations.insert(node_id, in_scope_decl);

  // Process all referenced nodes from this declaration if it is a function,
  // modifier, or contract
  match first_pass_decl {
    FirstPassDeclaration::FunctionMod {
      referenced_nodes, ..
    } => {
      for ref_node in referenced_nodes {
        process_tree_shake_declarations(
          ref_node.referenced_node,
          Some(ref_node.statement_node), // Pass the semantic block that contains the reference
          ReferenceProcessingMethod::Normal,
          first_pass_declarations,
          in_scope_declarations,
          mutations_map,
          visiting,
        )?;
      }
    }
    FirstPassDeclaration::Contract {
      base_contracts,
      other_contracts,
      public_members,
      referenced_nodes,
      ..
    } => {
      // Process base contracts with BaseContract context
      for base_contract_ref in base_contracts {
        process_tree_shake_declarations(
          base_contract_ref.referenced_node,
          Some(base_contract_ref.statement_node), // This contract references the base contract
          ReferenceProcessingMethod::ProcessAllContractMembers,
          first_pass_declarations,
          in_scope_declarations,
          mutations_map,
          visiting,
        )?;
      }

      // Process other contracts (using for, type references) with NoContext
      for other_contract_ref in other_contracts {
        process_tree_shake_declarations(
          other_contract_ref.referenced_node,
          Some(other_contract_ref.statement_node), // This contract references the other contract
          ReferenceProcessingMethod::Normal,
          first_pass_declarations,
          in_scope_declarations,
          mutations_map,
          visiting,
        )?;
      }

      // Process type references from state variable declarations
      for ref_node in referenced_nodes {
        process_tree_shake_declarations(
          ref_node.referenced_node,
          Some(ref_node.statement_node), // The VariableDeclaration node references the type
          ReferenceProcessingMethod::Normal,
          first_pass_declarations,
          in_scope_declarations,
          mutations_map,
          visiting,
        )?;
      }

      // Process public members
      if reference_processing_method
        == ReferenceProcessingMethod::ProcessAllContractMembers
      {
        for &public_member_id in public_members {
          process_tree_shake_declarations(
            public_member_id,
            None,
            ReferenceProcessingMethod::Normal,
            first_pass_declarations,
            in_scope_declarations,
            mutations_map,
            visiting,
          )?;
        }
      }
    }
    FirstPassDeclaration::Flat { .. } => (),
  };

  // Mark as fully processed
  visiting.remove(&node_id);

  Ok(())
}

// ============================================================================
// Ancestry Pass
// ============================================================================

/// Maps variable node_id -> Vec of ancestor variable node_ids (direct ancestors only)
pub type AncestorsMap = BTreeMap<i32, Vec<i32>>;

/// Maps variable node_id -> Vec of descendant variable node_ids
pub type DescendantsMap = BTreeMap<i32, Vec<i32>>;

/// Context for tracking function return parameters during ancestry collection.
/// Used when processing Return statements to link expression variables to return parameters.
struct AncestryContext {
  /// The return parameter node IDs for the current function being processed
  return_parameter_ids: Vec<i32>,
}

/// Recursively collects ancestry relationships from a node and its children.
fn collect_ancestry_from_node(
  node: &ASTNode,
  context: Option<&AncestryContext>,
  ancestors_map: &mut AncestorsMap,
) {
  match node {
    // Variable declaration with initializer (state variable or local)
    ASTNode::VariableDeclaration {
      node_id,
      value: Some(initial_value),
      ..
    } => {
      // Collect all variable references from the initial value expression
      let ancestor_ids = collect_variable_refs_from_expression(initial_value);
      add_ancestors(ancestors_map, *node_id, &ancestor_ids);

      // Recurse into the initial value
      collect_ancestry_from_node(initial_value, context, ancestors_map);
    }

    // Variable declaration statement (local variables with initializer)
    ASTNode::VariableDeclarationStatement {
      declarations,
      initial_value: Some(init_value),
      ..
    } => {
      // Check if this is a multi-return function call
      if let ASTNode::FunctionCall {
        referenced_return_declarations,
        ..
      } = init_value.as_ref()
      {
        if referenced_return_declarations.len() > 1
          && declarations.len() == referenced_return_declarations.len()
        {
          // Multi-return case: pair each declaration with its corresponding return declaration
          for (i, decl) in declarations.iter().enumerate() {
            if let ASTNode::VariableDeclaration { node_id, .. } = decl {
              // The return declaration is an ancestor of this variable
              add_ancestors(
                ancestors_map,
                *node_id,
                &[referenced_return_declarations[i]],
              );
            }
          }
          // Also recurse into the function call to process its arguments
          collect_ancestry_from_node(init_value, context, ancestors_map);
          // Recurse into declarations (they may have nested structures)
          for decl in declarations {
            collect_ancestry_from_node(decl, context, ancestors_map);
          }
          return;
        }
      }

      // Single variable or single-return function call case
      // Collect all variable references from the initial value
      let ancestor_ids = collect_variable_refs_from_expression(init_value);

      // Add ancestors to each declared variable
      for decl in declarations {
        if let ASTNode::VariableDeclaration { node_id, .. } = decl {
          add_ancestors(ancestors_map, *node_id, &ancestor_ids);
        }
      }

      // Recurse into initial value and declarations
      collect_ancestry_from_node(init_value, context, ancestors_map);
      for decl in declarations {
        collect_ancestry_from_node(decl, context, ancestors_map);
      }
    }

    // Assignment: RHS variables are ancestors of LHS base variable
    // Also handles index access on LHS (e.g., myMap[key] = val)
    ASTNode::Assignment {
      left_hand_side,
      right_hand_side,
      ..
    } => {
      if let Some(target_var_id) =
        extract_base_variable_reference(left_hand_side)
      {
        // Collect ancestors from RHS
        let mut ancestor_ids =
          collect_variable_refs_from_expression(right_hand_side);

        // Also collect index expressions from LHS (for mappings/arrays)
        collect_index_refs_from_lhs(left_hand_side, &mut ancestor_ids);

        add_ancestors(ancestors_map, target_var_id, &ancestor_ids);
      }

      // Recurse into both sides
      collect_ancestry_from_node(left_hand_side, context, ancestors_map);
      collect_ancestry_from_node(right_hand_side, context, ancestors_map);
    }

    // Function call with Argument nodes: argument variables are ancestors of parameter variables
    ASTNode::FunctionCall {
      arguments,
      expression,
      ..
    } => {
      for arg in arguments {
        if let ASTNode::Argument {
          parameter: Some(param_identifier),
          argument: arg_expr,
          ..
        } = arg
        {
          // The parameter identifier references the VariableDeclaration
          if let ASTNode::Identifier {
            referenced_declaration: param_var_id,
            ..
          } = param_identifier.as_ref()
          {
            let ancestor_ids = collect_variable_refs_from_expression(arg_expr);
            add_ancestors(ancestors_map, *param_var_id, &ancestor_ids);
          }
        }
        // Recurse into argument
        collect_ancestry_from_node(arg, context, ancestors_map);
      }

      // Recurse into expression
      collect_ancestry_from_node(expression, context, ancestors_map);
    }

    // Return statement: expression variables are ancestors of return parameter variables
    ASTNode::Return {
      expression: Some(return_expr),
      ..
    } => {
      // We need the return parameters from context or look them up
      // The function_return_parameters field is the node_id of the ParameterList
      // We need to find the actual parameter VariableDeclarations
      if let Some(ctx) = context {
        let ancestor_ids = collect_variable_refs_from_expression(return_expr);

        // Handle tuple returns: if return_expr is a TupleExpression, pair with return params
        if let ASTNode::TupleExpression { components, .. } =
          return_expr.as_ref()
        {
          if components.len() == ctx.return_parameter_ids.len() {
            // Pair each component with its corresponding return parameter
            for (i, component) in components.iter().enumerate() {
              let comp_ancestors =
                collect_variable_refs_from_expression(component);
              add_ancestors(
                ancestors_map,
                ctx.return_parameter_ids[i],
                &comp_ancestors,
              );
            }
          } else {
            // Fallback: all expression variables are ancestors of all return params
            for &ret_param_id in &ctx.return_parameter_ids {
              add_ancestors(ancestors_map, ret_param_id, &ancestor_ids);
            }
          }
        } else {
          // Single return value - all expression variables are ancestors of all return params
          // (typically there's only one return param in this case)
          for &ret_param_id in &ctx.return_parameter_ids {
            add_ancestors(ancestors_map, ret_param_id, &ancestor_ids);
          }
        }
      }
      // Note: If we don't have context, we can't resolve the return parameters.
      // This is handled by building context when entering functions.

      // Still recurse into the return expression
      collect_ancestry_from_node(return_expr, context, ancestors_map);
    }

    // Function definition: build context with return parameters and recurse
    ASTNode::FunctionDefinition {
      signature, body, ..
    } => {
      let return_param_ids = extract_return_parameter_ids(signature);
      let func_context = AncestryContext {
        return_parameter_ids: return_param_ids,
      };

      // Recurse into signature and body with the function context
      collect_ancestry_from_node(signature, Some(&func_context), ancestors_map);
      if let Some(body_node) = body {
        collect_ancestry_from_node(
          body_node,
          Some(&func_context),
          ancestors_map,
        );
      }
    }

    // Modifier definition: similar to function
    ASTNode::ModifierDefinition {
      signature, body, ..
    } => {
      // Modifiers don't have return parameters, but we still recurse
      collect_ancestry_from_node(signature, context, ancestors_map);
      collect_ancestry_from_node(body, context, ancestors_map);
    }

    // For all other nodes, just recurse into children
    _ => {
      for child in node.nodes() {
        collect_ancestry_from_node(child, context, ancestors_map);
      }
    }
  }
}

/// Extracts return parameter node IDs from a FunctionSignature
fn extract_return_parameter_ids(signature: &ASTNode) -> Vec<i32> {
  if let ASTNode::FunctionSignature {
    return_parameters, ..
  } = signature
  {
    if let ASTNode::ParameterList { parameters, .. } =
      return_parameters.as_ref()
    {
      return parameters
        .iter()
        .filter_map(|p| {
          if let ASTNode::VariableDeclaration { node_id, .. } = p {
            Some(*node_id)
          } else {
            None
          }
        })
        .collect();
    }
  }
  Vec::new()
}

/// Collects all variable reference node IDs from an expression.
/// Returns the referenced_declaration values (variable node IDs), not the expression node IDs.
fn collect_variable_refs_from_expression(node: &ASTNode) -> Vec<i32> {
  let mut refs = Vec::new();
  collect_variable_refs_recursive(node, &mut refs);
  refs
}

/// Recursive helper for collecting variable references
fn collect_variable_refs_recursive(node: &ASTNode, refs: &mut Vec<i32>) {
  match node {
    ASTNode::Identifier {
      referenced_declaration,
      ..
    }
    | ASTNode::IdentifierPath {
      referenced_declaration,
      ..
    } => {
      if !refs.contains(referenced_declaration) {
        refs.push(*referenced_declaration);
      }
    }

    ASTNode::MemberAccess {
      referenced_declaration: Some(ref_decl),
      expression,
      ..
    } => {
      if !refs.contains(ref_decl) {
        refs.push(*ref_decl);
      }
      // Also recurse into the expression (e.g., for chained access)
      collect_variable_refs_recursive(expression, refs);
    }

    ASTNode::MemberAccess {
      referenced_declaration: None,
      expression,
      ..
    } => {
      // No direct reference, but recurse into expression
      collect_variable_refs_recursive(expression, refs);
    }

    // Function calls: include the return declaration references
    ASTNode::FunctionCall {
      referenced_return_declarations,
      expression,
      arguments,
      ..
    } => {
      // Add the return declarations as ancestors
      for &ret_decl_id in referenced_return_declarations {
        if !refs.contains(&ret_decl_id) {
          refs.push(ret_decl_id);
        }
      }
      // Recurse into expression and arguments
      collect_variable_refs_recursive(expression, refs);
      for arg in arguments {
        collect_variable_refs_recursive(arg, refs);
      }
    }

    // For other nodes, recurse into children
    _ => {
      for child in node.nodes() {
        collect_variable_refs_recursive(child, refs);
      }
    }
  }
}

/// Collects variable references from index expressions in the LHS of an assignment.
/// For example, in `myMap[key1][key2] = val`, this collects key1 and key2.
fn collect_index_refs_from_lhs(lhs: &ASTNode, refs: &mut Vec<i32>) {
  match lhs {
    ASTNode::IndexAccess {
      base_expression,
      index_expression,
      ..
    } => {
      // Collect refs from the index expression
      if let Some(index_expr) = index_expression {
        let index_refs = collect_variable_refs_from_expression(index_expr);
        for ref_id in index_refs {
          if !refs.contains(&ref_id) {
            refs.push(ref_id);
          }
        }
      }
      // Recurse into base expression for nested index access
      collect_index_refs_from_lhs(base_expression, refs);
    }
    ASTNode::MemberAccess { expression, .. } => {
      // For member access like `obj.field[key] = val`, recurse into expression
      collect_index_refs_from_lhs(expression, refs);
    }
    _ => {}
  }
}

/// Adds ancestor variable IDs to the ancestors map for a target variable.
fn add_ancestors(
  ancestors_map: &mut AncestorsMap,
  target_id: i32,
  ancestor_ids: &[i32],
) {
  if ancestor_ids.is_empty() {
    return;
  }

  let entry = ancestors_map.entry(target_id).or_insert_with(Vec::new);
  for &ancestor_id in ancestor_ids {
    // Don't add self-references
    if ancestor_id != target_id && !entry.contains(&ancestor_id) {
      entry.push(ancestor_id);
    }
  }
}

/// Filters the ancestors map to only include in-scope variables and derives descendants.
/// Returns both the filtered ancestors map and the derived descendants map.
fn filter_and_derive_descendants(
  ancestors_map: &AncestorsMap,
  in_scope_declarations: &BTreeMap<i32, InScopeDeclaration>,
) -> (AncestorsMap, DescendantsMap) {
  let mut filtered_ancestors: AncestorsMap = BTreeMap::new();
  let mut descendants: DescendantsMap = BTreeMap::new();

  // Filter ancestors to only include in-scope variables
  for (&var_id, ancestor_ids) in ancestors_map {
    // Only include if the target variable is in scope
    if !in_scope_declarations.contains_key(&var_id) {
      continue;
    }

    // Filter ancestor IDs to only in-scope variables
    let filtered_ancestor_ids: Vec<i32> = ancestor_ids
      .iter()
      .filter(|&&aid| in_scope_declarations.contains_key(&aid))
      .copied()
      .collect();

    if !filtered_ancestor_ids.is_empty() {
      filtered_ancestors.insert(var_id, filtered_ancestor_ids.clone());

      // Build descendants: for each ancestor, this variable is a descendant
      for ancestor_id in filtered_ancestor_ids {
        descendants
          .entry(ancestor_id)
          .or_insert_with(Vec::new)
          .push(var_id);
      }
    }
  }

  (filtered_ancestors, descendants)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::solidity::parser::SourceLocation;

  fn dummy_src_location() -> SourceLocation {
    SourceLocation {
      start: None,
      length: None,
      index: None,
    }
  }

  // =========================================================================
  // Type Extraction Tests
  // =========================================================================

  #[test]
  fn test_parse_elementary_type_uint256() {
    let result = parse_elementary_type_name("uint256");
    assert_eq!(result, Some(ElementaryType::Uint { bits: 256 }));
  }

  #[test]
  fn test_parse_elementary_type_uint_default() {
    // "uint" without size defaults to uint256
    let result = parse_elementary_type_name("uint");
    assert_eq!(result, Some(ElementaryType::Uint { bits: 256 }));
  }

  #[test]
  fn test_parse_elementary_type_uint8() {
    let result = parse_elementary_type_name("uint8");
    assert_eq!(result, Some(ElementaryType::Uint { bits: 8 }));
  }

  #[test]
  fn test_parse_elementary_type_int256() {
    let result = parse_elementary_type_name("int256");
    assert_eq!(result, Some(ElementaryType::Int { bits: 256 }));
  }

  #[test]
  fn test_parse_elementary_type_int_default() {
    // "int" without size defaults to int256
    let result = parse_elementary_type_name("int");
    assert_eq!(result, Some(ElementaryType::Int { bits: 256 }));
  }

  #[test]
  fn test_parse_elementary_type_int8() {
    let result = parse_elementary_type_name("int8");
    assert_eq!(result, Some(ElementaryType::Int { bits: 8 }));
  }

  #[test]
  fn test_parse_elementary_type_address() {
    let result = parse_elementary_type_name("address");
    assert_eq!(result, Some(ElementaryType::Address));
  }

  #[test]
  fn test_parse_elementary_type_address_payable() {
    let result = parse_elementary_type_name("address payable");
    assert_eq!(result, Some(ElementaryType::AddressPayable));
  }

  #[test]
  fn test_parse_elementary_type_bool() {
    let result = parse_elementary_type_name("bool");
    assert_eq!(result, Some(ElementaryType::Bool));
  }

  #[test]
  fn test_parse_elementary_type_string() {
    let result = parse_elementary_type_name("string");
    assert_eq!(result, Some(ElementaryType::String));
  }

  #[test]
  fn test_parse_elementary_type_bytes() {
    let result = parse_elementary_type_name("bytes");
    assert_eq!(result, Some(ElementaryType::Bytes));
  }

  #[test]
  fn test_parse_elementary_type_bytes32() {
    let result = parse_elementary_type_name("bytes32");
    assert_eq!(result, Some(ElementaryType::FixedBytes(32)));
  }

  #[test]
  fn test_parse_elementary_type_bytes1() {
    let result = parse_elementary_type_name("bytes1");
    assert_eq!(result, Some(ElementaryType::FixedBytes(1)));
  }

  #[test]
  fn test_extract_solidity_type_elementary() {
    let node = ASTNode::ElementaryTypeName {
      node_id: 1,
      src_location: dummy_src_location(),
      name: "uint256".to_string(),
    };

    let result = extract_solidity_type(&node);
    assert_eq!(
      result,
      Some(SolidityType::Elementary(ElementaryType::Uint { bits: 256 }))
    );
  }

  #[test]
  fn test_extract_solidity_type_array() {
    let base_type = ASTNode::ElementaryTypeName {
      node_id: 2,
      src_location: dummy_src_location(),
      name: "uint256".to_string(),
    };

    let node = ASTNode::ArrayTypeName {
      node_id: 1,
      src_location: dummy_src_location(),
      base_type: Box::new(base_type),
    };

    let result = extract_solidity_type(&node);
    assert_eq!(
      result,
      Some(SolidityType::Array {
        base_type: Box::new(SolidityType::Elementary(ElementaryType::Uint {
          bits: 256
        })),
        length: None,
      })
    );
  }

  #[test]
  fn test_extract_solidity_type_mapping() {
    let key_type = ASTNode::ElementaryTypeName {
      node_id: 2,
      src_location: dummy_src_location(),
      name: "address".to_string(),
    };

    let value_type = ASTNode::ElementaryTypeName {
      node_id: 3,
      src_location: dummy_src_location(),
      name: "uint256".to_string(),
    };

    let node = ASTNode::Mapping {
      node_id: 1,
      src_location: dummy_src_location(),
      key_name: None,
      key_name_location: dummy_src_location(),
      key_type: Box::new(key_type),
      value_name: None,
      value_name_location: dummy_src_location(),
      value_type: Box::new(value_type),
    };

    let result = extract_solidity_type(&node);
    assert_eq!(
      result,
      Some(SolidityType::Mapping {
        key_type: Box::new(SolidityType::Elementary(ElementaryType::Address)),
        value_type: Box::new(SolidityType::Elementary(ElementaryType::Uint {
          bits: 256
        })),
      })
    );
  }

  #[test]
  fn test_extract_solidity_type_user_defined() {
    let path_node = ASTNode::IdentifierPath {
      node_id: 2,
      src_location: dummy_src_location(),
      name: "MyStruct".to_string(),
      name_locations: vec![],
      referenced_declaration: 100,
    };

    let node = ASTNode::UserDefinedTypeName {
      node_id: 1,
      src_location: dummy_src_location(),
      referenced_declaration: 100,
      path_node: Box::new(path_node),
    };

    let result = extract_solidity_type(&node);
    assert_eq!(
      result,
      Some(SolidityType::UserDefined {
        declaration_topic: topic::new_node_topic(&100),
      })
    );
  }

  // =========================================================================
  // Constraint Collection Tests
  // =========================================================================

  #[test]
  fn test_if_context_tracking() {
    // Test that IfContext correctly tracks condition node, branch, and variable refs
    let ctx = IfContext {
      condition_node: 42,
      in_true_branch: true,
      referenced_variable_nodes: vec![100, 101],
    };
    assert_eq!(ctx.condition_node, 42);
    assert!(ctx.in_true_branch);
    assert_eq!(ctx.referenced_variable_nodes, vec![100, 101]);

    let ctx_false = IfContext {
      condition_node: 43,
      in_true_branch: false,
      referenced_variable_nodes: vec![],
    };
    assert_eq!(ctx_false.condition_node, 43);
    assert!(!ctx_false.in_true_branch);
    assert!(ctx_false.referenced_variable_nodes.is_empty());
  }

  #[test]
  fn test_first_pass_revert_constraint_require() {
    // Test creating a require constraint with variable references
    let constraint = FirstPassRevertConstraint {
      statement_node: 100,
      conditions: vec![FirstPassRevertCondition {
        condition_node: 101,
        must_be_true: false, // require reverts when condition is false
      }],
      kind: RevertConstraintKind::Require,
      referenced_variable_nodes: vec![200, 201], // Variables in the condition
    };

    assert_eq!(constraint.statement_node, 100);
    assert_eq!(constraint.conditions.len(), 1);
    assert_eq!(constraint.conditions[0].condition_node, 101);
    assert!(!constraint.conditions[0].must_be_true);
    assert_eq!(constraint.kind, RevertConstraintKind::Require);
    assert_eq!(constraint.referenced_variable_nodes, vec![200, 201]);
  }

  #[test]
  fn test_first_pass_revert_constraint_nested_if() {
    // Test creating a revert constraint with nested if conditions
    // Simulating: if (a > 0) { if (b < 10) { revert(); } }
    let constraint = FirstPassRevertConstraint {
      statement_node: 200,
      conditions: vec![
        FirstPassRevertCondition {
          condition_node: 201, // a > 0
          must_be_true: true,
        },
        FirstPassRevertCondition {
          condition_node: 202, // b < 10
          must_be_true: true,
        },
      ],
      kind: RevertConstraintKind::Revert,
      referenced_variable_nodes: vec![300, 301], // a and b
    };

    assert_eq!(constraint.statement_node, 200);
    assert_eq!(constraint.conditions.len(), 2);
    assert!(constraint.conditions[0].must_be_true);
    assert!(constraint.conditions[1].must_be_true);
    assert_eq!(constraint.kind, RevertConstraintKind::Revert);
    assert_eq!(constraint.referenced_variable_nodes.len(), 2);
  }

  #[test]
  fn test_first_pass_revert_constraint_else_branch() {
    // Test creating a revert constraint in an else branch
    // Simulating: if (condition) { ... } else { revert(); }
    let constraint = FirstPassRevertConstraint {
      statement_node: 300,
      conditions: vec![FirstPassRevertCondition {
        condition_node: 301,
        must_be_true: false, // In else branch, condition must be false
      }],
      kind: RevertConstraintKind::Revert,
      referenced_variable_nodes: vec![400],
    };

    assert_eq!(constraint.conditions.len(), 1);
    assert!(!constraint.conditions[0].must_be_true);
    assert_eq!(constraint.referenced_variable_nodes, vec![400]);
  }

  #[test]
  fn test_elementary_type_is_numeric() {
    assert!(ElementaryType::Uint { bits: 256 }.is_numeric());
    assert!(ElementaryType::Int { bits: 8 }.is_numeric());
    assert!(!ElementaryType::Address.is_numeric());
    assert!(!ElementaryType::Bool.is_numeric());
    assert!(!ElementaryType::String.is_numeric());
  }

  #[test]
  fn test_elementary_type_is_address() {
    assert!(ElementaryType::Address.is_address());
    assert!(ElementaryType::AddressPayable.is_address());
    assert!(!ElementaryType::Uint { bits: 256 }.is_address());
    assert!(!ElementaryType::Bool.is_address());
  }
}
