use std::path::{Path, PathBuf};

pub mod project;
pub mod topic;

use std::collections::{BTreeMap, HashSet};

use crate::solidity::parser;

// ============================================================================
// Solidity Type System (for checker module)
// ============================================================================

/// Represents a Solidity type for use in the checker.
/// Contains enough detail to derive valid value ranges.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolidityType {
  /// Elementary types with full detail for value range derivation
  Elementary(ElementaryType),
  /// User-defined types reference the declaration topic
  UserDefined { declaration_topic: topic::Topic },
  /// Array types - length is Some for fixed-size arrays
  Array {
    base_type: Box<SolidityType>,
    length: Option<u64>,
  },
  /// Mapping types
  Mapping {
    key_type: Box<SolidityType>,
    value_type: Box<SolidityType>,
  },
  /// Function types
  Function {
    parameter_types: Vec<SolidityType>,
    return_types: Vec<SolidityType>,
  },
}

/// Elementary types with enough detail to derive value ranges.
/// Numbers include bit size so the checker can compute min/max values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementaryType {
  /// Boolean: range is {false, true}
  Bool,
  /// Address: 20 bytes, range is 0 to 2^160-1
  Address,
  /// Payable address: same range as Address
  AddressPayable,
  /// Fixed-size bytes: bytesN where N is 1-32
  /// Range is 0 to 2^(N*8)-1
  FixedBytes(u8),
  /// Dynamic bytes: no fixed range
  Bytes,
  /// String: no fixed numeric range
  String,
  /// Signed integer: intN where bits is 8, 16, 24, ... 256
  /// Range is -2^(bits-1) to 2^(bits-1)-1
  Int { bits: u16 },
  /// Unsigned integer: uintN where bits is 8, 16, 24, ... 256
  /// Range is 0 to 2^bits-1
  Uint { bits: u16 },
}

impl ElementaryType {
  /// Returns true if this type has a numeric value range
  pub fn is_numeric(&self) -> bool {
    matches!(
      self,
      ElementaryType::Int { .. } | ElementaryType::Uint { .. }
    )
  }

  /// Returns true if this type is an address
  pub fn is_address(&self) -> bool {
    matches!(
      self,
      ElementaryType::Address | ElementaryType::AddressPayable
    )
  }
}

// ============================================================================
// Revert Constraint Types (for checker module)
// ============================================================================

/// Represents a single condition in the path to a revert.
/// Multiple conditions form a chain when reverts are nested in if statements.
#[derive(Debug, Clone)]
pub struct RevertCondition {
  /// The topic of the condition expression (e.g., the if condition)
  pub condition_topic: topic::Topic,
  /// Whether the revert happens when this condition is true or false.
  /// For `if (cond) { revert }` -> must_be_true = true
  /// For `if (cond) { } else { revert }` -> must_be_true = false
  pub must_be_true: bool,
}

/// Represents a constraint from a require or revert statement.
#[derive(Debug, Clone)]
pub struct RevertConstraint {
  /// The topic of the revert or require statement node
  pub statement_topic: topic::Topic,
  /// The chain of conditions leading to this revert.
  /// Empty for unconditional reverts.
  /// For require(cond): single entry with cond and must_be_true=false (reverts when false)
  /// For nested if statements: multiple entries, all must hold for revert to occur
  pub conditions: Vec<RevertCondition>,
  /// The kind of revert statement
  pub kind: RevertConstraintKind,
  /// Variables referenced in the condition expressions.
  /// Used to index constraints by variable for UI lookup.
  pub referenced_variables: Vec<topic::Topic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RevertConstraintKind {
  /// require(condition) - reverts when condition is false
  Require,
  /// revert with enclosing if conditions
  Revert,
}

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
  // Contains the ASTs for a given file path
  pub asts: BTreeMap<ProjectPath, AST>,
  // Contains the node for a given topic
  pub nodes: BTreeMap<topic::Topic, Node>,
  // Contains the declaration for a given topic
  pub topic_metadata: BTreeMap<topic::Topic, TopicMetadata>,
  // Contains the function properties for a given topic
  pub function_properties: BTreeMap<topic::Topic, FunctionModProperties>,
  /// Maps variable topic IDs to their Solidity types (for checker module)
  pub variable_types: BTreeMap<topic::Topic, SolidityType>,
  /// Maps variable topics to constraint statement topics that reference them.
  /// Allows UI to show "constraints involving this variable".
  pub variable_constraints: BTreeMap<topic::Topic, Vec<topic::Topic>>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
  Global,
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
  SemanticBlock {
    container: ProjectPath,
    component: topic::Topic,
    member: topic::Topic,
    semantic_block: topic::Topic,
  },
}

pub fn add_to_scope(scope: &Scope, topic: topic::Topic) -> Scope {
  match scope {
    Scope::Global => Scope::Global, // Global scope cannot be nested
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
    } => Scope::SemanticBlock {
      container: container.clone(),
      component: component.clone(),
      member: member.clone(),
      semantic_block: topic,
    },
    Scope::SemanticBlock {
      container,
      component,
      member,
      ..
    } => Scope::SemanticBlock {
      container: container.clone(),
      component: component.clone(),
      member: member.clone(),
      semantic_block: topic,
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
  Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamedMutableTopicKind {
  StateVariable,
  LocalVariable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnnamedTopicKind {
  VariableMutation,
  Arithmetic,
  Comparison,
  Logical,
  Bitwise,
  Conditional,
  FunctionCall,
  TypeConversion,
  StructConstruction,
  NewExpression,
  SemanticBlock,
  Break,
  Continue,
  DoWhile,
  Emit,
  For,
  If,
  InlineAssembly,
  Placeholder,
  Return,
  Revert,
  Try,
  UncheckedBlock,
  While,
  Reference,
  MutableReference,
  Signature,
  DocumentationSection,
  DocumentationParagraph,
  Other,
}

#[derive(Debug, Clone)]
pub enum NamedTopicVisibility {
  Public,
  Private,
  Internal,
  External,
}

/// Groups references to a declaration by the contract where they occur.
/// Contains both contract-level references (inheritance, using-for) and
/// member-level references (inside functions/modifiers).
#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceGroup {
  /// The contract/interface/library where these references occur
  contract: topic::Topic,
  /// Whether this contract is defined in one of the audit's in-scope files
  is_in_scope: bool,
  /// References at the contract scope level (inheritance, using-for directives, state variable types)
  contract_references: Vec<topic::Topic>,
  /// References within members of the contract (functions, modifiers)
  member_references: Vec<MemberReferenceGroup>,
}

impl ReferenceGroup {
  pub fn new(
    contract: topic::Topic,
    is_in_scope: bool,
    contract_references: Vec<topic::Topic>,
    member_references: Vec<MemberReferenceGroup>,
  ) -> Self {
    Self {
      contract,
      is_in_scope,
      contract_references,
      member_references,
    }
  }

  pub fn contract(&self) -> &topic::Topic {
    &self.contract
  }

  pub fn is_in_scope(&self) -> bool {
    self.is_in_scope
  }

  pub fn contract_references(&self) -> &[topic::Topic] {
    &self.contract_references
  }

  pub fn member_references(&self) -> &[MemberReferenceGroup] {
    &self.member_references
  }
}

/// Groups references within a single member (function/modifier) of a contract.
#[derive(Debug, Clone, PartialEq)]
pub struct MemberReferenceGroup {
  /// The function/modifier containing these references
  member: topic::Topic,
  /// References within this member
  references: Vec<topic::Topic>,
}

impl MemberReferenceGroup {
  pub fn new(member: topic::Topic, references: Vec<topic::Topic>) -> Self {
    Self { member, references }
  }

  pub fn member(&self) -> &topic::Topic {
    &self.member
  }

  pub fn references(&self) -> &[topic::Topic] {
    &self.references
  }
}

#[derive(Debug, Clone)]
pub enum TopicMetadata {
  NamedTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: NamedTopicKind,
    name: String,
    visibility: NamedTopicVisibility,
    references: Vec<ReferenceGroup>,
    /// References derived from recursively traversing all ancestors and descendants.
    /// Contains grouped references showing where all transitive ancestry-related
    /// variables are declared/referenced.
    expanded_references: Vec<ReferenceGroup>,
    /// Variables that contribute to this variable's value (e.g., RHS of assignments,
    /// function arguments that flow into parameters, return expression variables).
    /// Only populated for variable declarations.
    ancestors: Vec<topic::Topic>,
    /// Variables whose values are derived from this variable.
    /// Only populated for variable declarations.
    descendants: Vec<topic::Topic>,
    /// Variables that appear together with this variable in comparison, arithmetic,
    /// or bitwise binary operations, or as alternatives in conditional (ternary) expressions.
    /// Only populated for variable declarations.
    relatives: Vec<topic::Topic>,
  },
  NamedMutableTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: NamedMutableTopicKind,
    name: String,
    visibility: parser::VariableVisibility,
    references: Vec<ReferenceGroup>,
    /// References derived from recursively traversing all ancestors and descendants.
    /// Contains grouped references showing where all transitive ancestry-related
    /// variables are declared/referenced.
    expanded_references: Vec<ReferenceGroup>,
    /// The assignment or unary operation nodes that mutate this variable
    mutations: Vec<topic::Topic>,
    /// Variables that contribute to this variable's value (e.g., RHS of assignments,
    /// function arguments that flow into parameters, return expression variables).
    ancestors: Vec<topic::Topic>,
    /// Variables whose values are derived from this variable.
    descendants: Vec<topic::Topic>,
    /// Variables that appear together with this variable in comparison, arithmetic,
    /// or bitwise binary operations, or as alternatives in conditional (ternary) expressions.
    relatives: Vec<topic::Topic>,
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
      TopicMetadata::NamedTopic { scope, .. }
      | TopicMetadata::NamedMutableTopic { scope, .. }
      | TopicMetadata::UnnamedTopic { scope, .. } => scope,
    }
  }

  pub fn name(&self) -> &str {
    match self {
      TopicMetadata::NamedTopic { name, .. }
      | TopicMetadata::NamedMutableTopic { name, .. } => name,
      TopicMetadata::UnnamedTopic { topic, .. } => topic.id(),
    }
  }

  pub fn topic(&self) -> &topic::Topic {
    match self {
      TopicMetadata::NamedTopic { topic, .. }
      | TopicMetadata::NamedMutableTopic { topic, .. }
      | TopicMetadata::UnnamedTopic { topic, .. } => topic,
    }
  }

  pub fn references(&self) -> &[ReferenceGroup] {
    match self {
      TopicMetadata::NamedTopic { references, .. }
      | TopicMetadata::NamedMutableTopic { references, .. } => references,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  pub fn expanded_references(&self) -> &[ReferenceGroup] {
    match self {
      TopicMetadata::NamedTopic {
        expanded_references,
        ..
      }
      | TopicMetadata::NamedMutableTopic {
        expanded_references,
        ..
      } => expanded_references,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  pub fn ancestors(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { ancestors, .. }
      | TopicMetadata::NamedMutableTopic { ancestors, .. } => ancestors,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  pub fn descendants(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { descendants, .. }
      | TopicMetadata::NamedMutableTopic { descendants, .. } => descendants,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  pub fn relatives(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { relatives, .. }
      | TopicMetadata::NamedMutableTopic { relatives, .. } => relatives,
      TopicMetadata::UnnamedTopic { .. } => &[],
    }
  }

  /// Returns the qualified name of the declaration
  /// Format: component::member::statement::name, component::member::name, component::name, or name
  /// Uses the declaration names from the scope components, falling back to topic IDs if not found
  pub fn qualified_name(&self, audit_data: &AuditData) -> String {
    match &self.scope() {
      Scope::Global => self.name().to_string(),
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
      Scope::SemanticBlock {
        component,
        member,
        semantic_block: statement,
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
    // Topic IDs of the declarations of the function revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<topic::Topic>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<topic::Topic>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<topic::Topic>,
    /// Full constraint information for the checker module
    revert_constraints: Vec<RevertConstraint>,
  },
  ModifierProperties {
    // Topic IDs of the declarations of the modifier revert nodes. This is either
    // the error call for a revert statement, or the literal string node passed
    // as the second argument to a require call
    reverts: Vec<topic::Topic>,
    // Topic IDs of the declarations of the functions called
    calls: Vec<topic::Topic>,
    // Topic IDs of the declarations of the state variables mutated
    mutations: Vec<topic::Topic>,
    /// Full constraint information for the checker module
    revert_constraints: Vec<RevertConstraint>,
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
  let mut topic_metadata = BTreeMap::new();

  // Pre-populate with Solidity globals
  // keccak256 function with node_id -8
  let keccak256_topic = topic::new_node_topic(&-8);
  topic_metadata.insert(
    keccak256_topic.clone(),
    TopicMetadata::NamedTopic {
      topic: keccak256_topic,
      scope: Scope::Global,
      kind: NamedTopicKind::Builtin,
      visibility: NamedTopicVisibility::Public,
      name: "keccak256".to_string(),
      references: Vec::new(),
      expanded_references: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
    },
  );

  // type() function with node_id -27
  let type_topic = topic::new_node_topic(&-27);
  topic_metadata.insert(
    type_topic.clone(),
    TopicMetadata::NamedTopic {
      topic: type_topic,
      scope: Scope::Global,
      kind: NamedTopicKind::Builtin,
      visibility: NamedTopicVisibility::Public,
      name: "type".to_string(),
      references: Vec::new(),
      expanded_references: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
    },
  );

  // this keyword with node_id -28
  let this_topic = topic::new_node_topic(&-28);
  topic_metadata.insert(
    this_topic.clone(),
    TopicMetadata::NamedTopic {
      topic: this_topic,
      scope: Scope::Global,
      kind: NamedTopicKind::Builtin,
      visibility: NamedTopicVisibility::Public,
      name: "this".to_string(),
      references: Vec::new(),
      expanded_references: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
    },
  );

  AuditData {
    audit_name,
    in_scope_files,
    asts: BTreeMap::new(),
    nodes: BTreeMap::new(),
    topic_metadata,
    function_properties: BTreeMap::new(),
    variable_types: BTreeMap::new(),
    variable_constraints: BTreeMap::new(),
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
