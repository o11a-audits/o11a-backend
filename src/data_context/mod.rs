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
  pub in_scope_files: HashSet<String>,
  pub nodes: BTreeMap<String, Node>,
  pub declarations: BTreeMap<String, Declaration>,
  pub references: BTreeMap<String, Vec<String>>,
  pub function_properties: BTreeMap<String, FunctionModProperties>,
  pub source_content: BTreeMap<String, String>,
}

pub enum Node {
  Solidity(crate::solidity::parser::ASTNode),
  Documentation(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
  pub container: String,         // Source file path
  pub component: Option<String>, // Contract topic ID
  pub member: Option<String>,    // Function topic ID
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
