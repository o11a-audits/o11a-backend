use crate::core::topic;
use std::collections::{BTreeMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionKind {
  Constructor,
  Function,
  Fallback,
  Receive,
  FreeFunction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContractKind {
  Contract,
  Library,
  Abstract,
  Interface,
}

pub struct DataContext {
  // A list of files that are in scope for the current audit
  pub in_scope_files: HashSet<String>,
  // Contains the content of the original source file for a given file path
  pub source_content: BTreeMap<String, String>,
  // Contains the ASTs for a given file path
  pub asts: BTreeMap<String, AST>,
  pub nodes: BTreeMap<topic::Topic, Node>,
  pub declarations: BTreeMap<topic::Topic, Declaration>,
  pub references: BTreeMap<topic::Topic, Vec<topic::Topic>>,
  pub function_properties: BTreeMap<topic::Topic, FunctionModProperties>,
}

pub enum Node {
  Solidity(crate::solidity::parser::ASTNode),
  Documentation(crate::documentation::parser::DocumentationNode),
}

pub enum AST {
  Solidity(crate::solidity::parser::SolidityAST),
  Documentation(crate::documentation::parser::DocumentationAST),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub container: String,               // Source file path
  pub component: Option<topic::Topic>, // Contract topic ID
  pub member: Option<topic::Topic>,    // Function topic ID
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationKind {
  Contract(ContractKind),
  Function(FunctionKind),
  Modifier,
  Event,
  Error,
  Struct,
  Enum,
  EnumMember,
  Constant,
  StateVariable,
  LocalVariable,
  DocumentationSection,
  DocumentationParagraph,
}

#[derive(Debug, Clone)]
pub struct Declaration {
  pub topic: topic::Topic,
  pub declaration_kind: DeclarationKind,
  pub name: String,
  pub scope: Scope,
}

impl Declaration {
  /// Returns the qualified name of the declaration
  /// Format: component.member.name or member.name or name
  /// Uses the declaration names from the scope components, not topic IDs
  pub fn qualified_name(&self, data_context: &DataContext) -> String {
    let mut parts = Vec::new();

    // Add component name if it exists
    if let Some(component_topic) = &self.scope.component {
      if let Some(component_decl) =
        data_context.declarations.get(component_topic)
      {
        parts.push(component_decl.name.clone());
      }
    }

    // Add member name if it exists
    if let Some(member_topic) = &self.scope.member {
      if let Some(member_decl) = data_context.declarations.get(member_topic) {
        parts.push(member_decl.name.clone());
      }
    }

    // Add the declaration's own name
    parts.push(self.name.clone());

    parts.join(".")
  }
}

pub enum FunctionModProperties {
  FunctionProperties {
    // Topic IDs of the local declarations of the function parameters
    parameters: Vec<topic::Topic>,
    // Topic IDs of the declarations of the function return values
    returns: Vec<topic::Topic>,
    // Topic IDs of the declarations of the function revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<topic::Topic>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<topic::Topic>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<topic::Topic>,
  },
  ModifierProperties {
    // Topic IDs of the local declarations of the modifier parameters
    parameters: Vec<topic::Topic>,
    // Topic IDs of the declarations of the modifier revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<topic::Topic>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<topic::Topic>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<topic::Topic>,
  },
}
