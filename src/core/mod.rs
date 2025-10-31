use std::path::Path;

pub mod project;
pub mod topic;

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

/// Contains all data for a single audit
pub struct AuditData {
  // The name of the audit being audited, like "Chainlink"
  pub audit_name: String,
  // A list of files that are in scope for this audit
  pub in_scope_files: HashSet<ProjectPath>,
  // Contains the content of the original source file for a given file path
  pub source_content: BTreeMap<ProjectPath, String>,
  // Contains the ASTs for a given file path
  pub asts: BTreeMap<ProjectPath, AST>,
  // Contains the node for a given topic
  pub nodes: BTreeMap<topic::Topic, Node>,
  // Contains the declaration for a given topic
  pub declarations: BTreeMap<topic::Topic, Declaration>,
  // Contains the references for a given topic
  pub references: BTreeMap<topic::Topic, Vec<topic::Topic>>,
  // Contains the function properties for a given topic
  pub function_properties: BTreeMap<topic::Topic, FunctionModProperties>,
}

pub struct DataContext {
  // Map of audit_id to audit data
  pub audits: BTreeMap<String, AuditData>,
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
  pub container: ProjectPath,
  pub component: Option<topic::Topic>,
  pub member: Option<topic::Topic>,
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
  pub fn qualified_name(&self, audit_data: &AuditData) -> String {
    let mut parts = Vec::new();

    // Add component name if it exists
    if let Some(component_topic) = &self.scope.component {
      if let Some(component_decl) = audit_data.declarations.get(component_topic)
      {
        parts.push(component_decl.name.clone());
      }
    }

    // Add member name if it exists
    if let Some(member_topic) = &self.scope.member {
      if let Some(member_decl) = audit_data.declarations.get(member_topic) {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
/// This type represents a path within a project, making sure that it is
/// a relative path to the project root.
pub struct ProjectPath {
  pub file_path: String,
}

pub fn new_project_path(
  file_path: &String,
  project_root: &Path,
) -> ProjectPath {
  new_project_path_from_path(Path::new(file_path), project_root)
}

pub fn new_project_path_from_path(
  file_path: &Path,
  project_root: &Path,
) -> ProjectPath {
  // If the file path is already relative, use it as-is
  if file_path.is_relative() {
    return ProjectPath {
      file_path: file_path.to_string_lossy().to_string(),
    };
  }

  // If the file path is absolute, try to strip the project path prefix
  let relative_path = file_path
    .strip_prefix(project_root)
    .unwrap_or(file_path)
    .to_string_lossy()
    .to_string();

  ProjectPath {
    file_path: relative_path,
  }
}

pub fn load_in_scope_files(
  project_root: &Path,
) -> Result<HashSet<ProjectPath>, String> {
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
      let project_path = new_project_path(&line.to_string(), project_root);
      in_scope_files.insert(project_path);
    }
  }

  Ok(in_scope_files)
}

/// Reads the first line of the "name.txt" file in the project root
pub fn load_audit_name(project_root: &Path) -> Result<String, String> {
  let name_file = project_root.join("name.txt");
  if !name_file.exists() {
    return Err("name.txt file not found in project root".to_string());
  }

  let content = std::fs::read_to_string(&name_file)
    .map_err(|e| format!("Failed to read name.txt: {}", e))?;

  let audit_name = content
    .lines()
    .next()
    .ok_or_else(|| "name.txt is empty".to_string())
    .map_err(|_| "Failed to parse name.txt".to_string())?
    .trim()
    .to_string();

  if audit_name.is_empty() {
    return Err("First line of name.txt is empty".to_string());
  }

  Ok(audit_name)
}

pub fn new_audit_data(
  audit_name: String,
  in_scope_files: HashSet<ProjectPath>,
) -> AuditData {
  AuditData {
    audit_name,
    in_scope_files,
    source_content: BTreeMap::new(),
    asts: BTreeMap::new(),
    nodes: BTreeMap::new(),
    declarations: BTreeMap::new(),
    references: BTreeMap::new(),
    function_properties: BTreeMap::new(),
  }
}

pub fn new_data_context() -> DataContext {
  DataContext {
    audits: BTreeMap::new(),
  }
}

impl DataContext {
  /// Creates a new audit and returns true if successful, false if audit already exists
  pub fn create_audit(
    &mut self,
    audit_id: String,
    audit_name: String,
    in_scope_files: HashSet<ProjectPath>,
  ) -> bool {
    if self.audits.contains_key(&audit_id) {
      return false;
    }
    self
      .audits
      .insert(audit_id, new_audit_data(audit_name, in_scope_files));
    true
  }

  /// Gets a reference to an audit's data
  pub fn get_audit(&self, audit_id: &str) -> Option<&AuditData> {
    self.audits.get(audit_id)
  }

  /// Gets a mutable reference to an audit's data
  pub fn get_audit_mut(&mut self, audit_id: &str) -> Option<&mut AuditData> {
    self.audits.get_mut(audit_id)
  }

  /// Removes an audit and returns true if it existed
  pub fn delete_audit(&mut self, audit_id: &str) -> bool {
    self.audits.remove(audit_id).is_some()
  }

  /// Lists all audit IDs
  pub fn list_audits(&self) -> Vec<String> {
    self.audits.keys().cloned().collect()
  }
}
