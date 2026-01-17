use std::path::{Path, PathBuf};

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
  pub topic_metadata: BTreeMap<topic::Topic, TopicMetadata>,
  // Contains the function properties for a given topic
  pub function_properties: BTreeMap<topic::Topic, FunctionModProperties>,
  // Contains the variable properties for a given topic
  pub variable_properties: BTreeMap<topic::Topic, VariableProperties>,
}

pub struct DataContext {
  // Map of audit_id to audit data
  pub audits: BTreeMap<String, AuditData>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
  Solidity(crate::solidity::parser::ASTNode),
  Documentation(crate::documentation::parser::DocumentationNode),
}

pub enum AST {
  Solidity(crate::solidity::parser::SolidityAST),
  Documentation(crate::documentation::parser::DocumentationAST),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
  Container {
    container: ProjectPath,
  },
  Component {
    container: ProjectPath,
    component: topic::Topic,
  },
  Member {
    container: ProjectPath,
    component: topic::Topic,
    member: topic::Topic,
  },
  Statement {
    container: ProjectPath,
    component: topic::Topic,
    member: topic::Topic,
    statement: topic::Topic,
  },
}

pub fn add_to_scope(scope: &Scope, topic: topic::Topic) -> Scope {
  match scope {
    Scope::Container { container } => Scope::Component {
      container: container.clone(),
      component: topic,
    },
    Scope::Component {
      container,
      component,
    } => Scope::Member {
      container: container.clone(),
      component: component.clone(),
      member: topic,
    },
    Scope::Member {
      container,
      component,
      member,
    } => Scope::Statement {
      container: container.clone(),
      component: component.clone(),
      member: member.clone(),
      statement: topic,
    },
    Scope::Statement {
      container,
      component,
      member,
      ..
    } => Scope::Statement {
      container: container.clone(),
      component: component.clone(),
      member: member.clone(),
      statement: topic,
    },
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariableMutability {
  Mutable,
  Immutable,
  Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamedTopicKind {
  Contract(ContractKind),
  Function(FunctionKind),
  Modifier,
  Event,
  Error,
  Struct,
  Enum,
  EnumMember,
  StateVariable(VariableMutability),
  LocalVariable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnnamedTopicKind {
  OperatorInvocation,
  DocumentationSection,
  DocumentationParagraph,
}

#[derive(Debug, Clone)]
pub enum TopicMetadata {
  NamedTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: NamedTopicKind,
    name: String,
    references: Vec<topic::Topic>,
  },
  UnnamedTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: UnnamedTopicKind,
  },
}

impl TopicMetadata {
  pub fn scope(&self) -> &Scope {
    match self {
      TopicMetadata::NamedTopic { scope, .. } => scope,
      TopicMetadata::UnnamedTopic { scope, .. } => scope,
    }
  }

  pub fn name(&self) -> &str {
    match self {
      TopicMetadata::NamedTopic { name, .. } => name,
      TopicMetadata::UnnamedTopic { topic, .. } => topic.id(),
    }
  }

  pub fn topic(&self) -> &topic::Topic {
    match self {
      TopicMetadata::NamedTopic { topic, .. } => topic,
      TopicMetadata::UnnamedTopic { topic, .. } => topic,
    }
  }

  pub fn references(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { references, .. } => references,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  /// Returns the qualified name of the declaration
  /// Format: component::member::statement::name, component::member::name, component::name, or name
  /// Uses the declaration names from the scope components, falling back to topic IDs if not found
  pub fn qualified_name(&self, audit_data: &AuditData) -> String {
    match &self.scope() {
      Scope::Container { .. } => self.name().to_string(),
      Scope::Component { component, .. } => {
        let component_name = audit_data
          .topic_metadata
          .get(component)
          .map(|d| d.name())
          .unwrap_or_else(|| component.id());
        format!("{}::{}", component_name, self.name())
      }
      Scope::Member {
        component, member, ..
      } => {
        let component_name = audit_data
          .topic_metadata
          .get(component)
          .map(|d| d.name())
          .unwrap_or_else(|| component.id());
        let member_name = audit_data
          .topic_metadata
          .get(member)
          .map(|d| d.name())
          .unwrap_or_else(|| member.id());
        format!("{}::{}::{}", component_name, member_name, self.name())
      }
      Scope::Statement {
        component,
        member,
        statement,
        ..
      } => {
        let component_name = audit_data
          .topic_metadata
          .get(component)
          .map(|d| d.name())
          .unwrap_or_else(|| component.id());
        let member_name = audit_data
          .topic_metadata
          .get(member)
          .map(|d| d.name())
          .unwrap_or_else(|| member.id());
        let statement_name = audit_data
          .topic_metadata
          .get(statement)
          .map(|d| d.name())
          .unwrap_or_else(|| statement.id());
        format!(
          "{}::{}::{}::{}",
          component_name,
          member_name,
          statement_name,
          self.name()
        )
      }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableProperties {
  // The statements where the variable is mutated
  pub mutations: Vec<topic::Topic>,
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
  // Convert relative paths to absolute by joining with project root
  let absolute_path = if file_path.is_relative() {
    project_root.join(file_path)
  } else {
    file_path.to_path_buf()
  };

  // Normalize the path by removing "." and ".." components
  let normalized = normalize_path(&absolute_path);

  // Strip the project root prefix to get a clean relative path
  let relative_path = normalized
    .strip_prefix(project_root)
    .unwrap_or(&normalized)
    .to_string_lossy()
    .to_string();

  ProjectPath {
    file_path: relative_path,
  }
}

pub fn project_path_to_absolute_path(
  project_path: &ProjectPath,
  project_root: &Path,
) -> PathBuf {
  project_root.join(&project_path.file_path)
}

/// Normalizes a path by resolving "." and ".." components
/// This is similar to canonicalize but doesn't require the path to exist
fn normalize_path(path: &Path) -> PathBuf {
  let mut components = Vec::new();

  for component in path.components() {
    match component {
      std::path::Component::CurDir => {
        // Skip "." components
      }
      std::path::Component::ParentDir => {
        // Remove the last component for ".."
        if !components.is_empty() {
          components.pop();
        }
      }
      _ => {
        // Add normal components (RootDir, Prefix, Normal)
        components.push(component);
      }
    }
  }

  components.iter().collect()
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
    topic_metadata: BTreeMap::new(),
    function_properties: BTreeMap::new(),
    variable_properties: BTreeMap::new(),
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

#[cfg(test)]
mod tests {
  use super::*;
  use std::path::PathBuf;

  #[test]
  fn test_new_project_path_strips_dot_slash() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = String::from("./src/my.sol");

    let result = new_project_path(&file_path, &project_root);

    assert_eq!(result.file_path, "src/my.sol");
  }

  #[test]
  fn test_new_project_path_from_path_strips_dot_slash() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = Path::new("./src/my.sol");

    let result = new_project_path_from_path(file_path, &project_root);

    assert_eq!(result.file_path, "src/my.sol");
  }

  #[test]
  fn test_new_project_path_handles_simple_relative() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = String::from("src/my.sol");

    let result = new_project_path(&file_path, &project_root);

    assert_eq!(result.file_path, "src/my.sol");
  }

  #[test]
  fn test_new_project_path_handles_absolute() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = String::from("/home/user/project/src/my.sol");

    let result = new_project_path(&file_path, &project_root);

    assert_eq!(result.file_path, "src/my.sol");
  }

  #[test]
  fn test_new_project_path_handles_parent_directory() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = String::from("./src/../contracts/my.sol");

    let result = new_project_path(&file_path, &project_root);

    assert_eq!(result.file_path, "contracts/my.sol");
  }

  #[test]
  fn test_new_project_path_handles_nested_dot_slash() {
    let project_root = PathBuf::from("/home/user/project");
    let file_path = String::from("./src/./contracts/./my.sol");

    let result = new_project_path(&file_path, &project_root);

    assert_eq!(result.file_path, "src/contracts/my.sol");
  }
}
