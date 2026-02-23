use std::path::{Path, PathBuf};

pub mod project;
pub mod topic;

use std::collections::{BTreeMap, HashMap, HashSet};

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

// ============================================================================
// Block Annotation Types
// ============================================================================

/// Describes the annotation on a containing block layer — either a control flow
/// statement whose body is this block, or an annotated block type like
/// `unchecked` or `assembly`.
///
/// Branch information is encoded directly in the kind — only `If` has branches,
/// so this avoids a disjoint field that would be meaningless for other kinds.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockAnnotation {
  /// The topic of the annotating node (the control flow statement or
  /// the annotated block itself).
  pub topic: topic::Topic,
  pub kind: BlockAnnotationKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockAnnotationKind {
  // Control flow
  If(ControlFlowBranch),
  For,
  While,
  DoWhile,
  // Annotated blocks
  Unchecked,
  InlineAssembly,
}

/// Branchless kind for the TopicMetadata::ControlFlow variant.
/// Unlike BlockAnnotationKind (which encodes branch info for scope tracking),
/// this simply identifies the statement type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlFlowStatementKind {
  If,
  For,
  While,
  DoWhile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlowBranch {
  True,
  False,
}

/// One layer in the containing block nesting chain.
/// Pairs a semantic block with an optional annotation describing what
/// kind of block it is (control flow body, unchecked, assembly, etc.).
#[derive(Debug, Clone)]
pub struct ContainingBlockLayer {
  /// The semantic block at this nesting level.
  pub block: topic::Topic,
  /// The annotation on this block layer, if any.
  /// None for plain semantic blocks with no governing statement or keyword.
  pub annotation: Option<BlockAnnotation>,
}

// ============================================================================
// Revert Info Types
// ============================================================================

/// Simple revert/require statement info stored on FunctionModProperties.
#[derive(Debug, Clone)]
pub struct RevertInfo {
  pub topic: topic::Topic,
  pub kind: RevertConstraintKind,
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
  /// Pre-computed name indexes for fast topic lookup by name.
  /// Built after all topic_metadata insertions are complete.
  pub name_index: TopicNameIndex,
}

/// Pre-computed name indexes for fast topic lookup by name.
/// Built once after all topic_metadata insertions are complete.
pub struct TopicNameIndex {
  by_qualified_name: HashMap<String, topic::Topic>,
  by_simple_name: HashMap<String, topic::Topic>,
}

impl TopicNameIndex {
  pub fn empty() -> Self {
    TopicNameIndex {
      by_qualified_name: HashMap::new(),
      by_simple_name: HashMap::new(),
    }
  }

  pub fn build(audit_data: &AuditData) -> Self {
    let mut by_qualified_name = HashMap::new();
    let mut simple_name_candidates: HashMap<String, Vec<topic::Topic>> =
      HashMap::new();

    for (topic, metadata) in &audit_data.topic_metadata {
      if let Some(qname) = metadata.qualified_name(audit_data) {
        by_qualified_name.insert(qname, topic.clone());
      }

      if let Some(sname) = metadata.name() {
        simple_name_candidates
          .entry(sname.to_string())
          .or_default()
          .push(topic.clone());
      }
    }

    let by_simple_name = simple_name_candidates
      .into_iter()
      .filter_map(|(name, topics)| {
        if topics.len() == 1 {
          Some((name, topics.into_iter().next().unwrap()))
        } else {
          None
        }
      })
      .collect();

    TopicNameIndex {
      by_qualified_name,
      by_simple_name,
    }
  }

  pub fn get_by_qualified_name(&self, name: &str) -> Option<&topic::Topic> {
    self.by_qualified_name.get(name)
  }

  pub fn get_by_simple_name(&self, name: &str) -> Option<&topic::Topic> {
    self.by_simple_name.get(name)
  }

  pub fn qualified_names(&self) -> Vec<&str> {
    self.by_qualified_name.keys().map(|s| s.as_str()).collect()
  }
}

/// Cached static parts of a topic view (AST-derived, never invalidated).
#[derive(Debug, Clone)]
pub struct CachedTopicView {
  pub topic_panel_html: String,
  pub expanded_references_panel_html: String,
  pub breadcrumb_html: String,
  pub highlight_css: String,
}

pub struct DataContext {
  // Map of audit_id to audit data
  pub audits: BTreeMap<String, AuditData>,
  /// Cached rendered HTML keyed by (audit_id, topic_id)
  pub source_text_cache: HashMap<String, HashMap<String, String>>,
  /// Cached topic view static panels keyed by (audit_id, topic_id).
  /// These are purely AST-derived and never invalidated.
  pub topic_view_cache: HashMap<String, HashMap<String, CachedTopicView>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
  Solidity(crate::solidity::parser::ASTNode),
  Documentation(crate::documentation::parser::DocumentationNode),
}

impl Node {
  /// Returns the source location start (byte offset) for this node.
  pub fn source_location_start(&self) -> Option<usize> {
    match self {
      Node::Solidity(ast_node) => ast_node.src_location().start,
      Node::Documentation(doc_node) => doc_node.position(),
    }
  }
}

pub enum AST {
  Solidity(crate::solidity::parser::SolidityAST),
  Documentation(crate::documentation::parser::DocumentationAST),
}

#[derive(Debug, Clone)]
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
    /// When the node is inside a member's signature, this holds the
    /// containing signature list node (e.g. the ParameterList for parameters
    /// or return values, or the ModifierList for modifier specifiers).
    /// None for nodes that are not inside a signature.
    signature_container: Option<topic::Topic>,
  },
  ContainingBlock {
    container: ProjectPath,
    component: topic::Topic,
    member: topic::Topic,
    containing_blocks: Vec<ContainingBlockLayer>,
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
      signature_container: None,
    },
    Scope::Member {
      container,
      component,
      member,
      ..
    } => {
      let mut containing_blocks = Vec::new();
      containing_blocks.push(ContainingBlockLayer {
        block: topic,
        annotation: None,
      });
      Scope::ContainingBlock {
        container: container.clone(),
        component: component.clone(),
        member: member.clone(),
        containing_blocks,
      }
    }
    Scope::ContainingBlock {
      container,
      component,
      member,
      containing_blocks,
    } => {
      let mut containing_blocks = containing_blocks.clone();
      containing_blocks.push(ContainingBlockLayer {
        block: topic,
        annotation: None,
      });
      Scope::ContainingBlock {
        container: container.clone(),
        component: component.clone(),
        member: member.clone(),
        containing_blocks,
      }
    }
  }
}

/// Attaches an annotation to the innermost containing block layer.
/// Used when a control flow statement or annotated block (unchecked, assembly)
/// is encountered within a semantic block.
///
/// Panics if the scope is not `ContainingBlock` (annotated blocks cannot
/// exist outside a semantic block) or if the innermost layer already has
/// an annotation (each block has at most one annotation on a nesting path).
pub fn add_annotation_to_scope(
  scope: &Scope,
  annotation: BlockAnnotation,
) -> Scope {
  match scope {
    Scope::ContainingBlock {
      container,
      component,
      member,
      containing_blocks,
    } => {
      let last = containing_blocks
        .last()
        .expect("ContainingBlock scope must have at least one layer");
      assert!(
        last.annotation.is_none(),
        "Invariant violation: innermost containing block layer already has an annotation.\n\
         Existing annotation: {:?}\n\
         New annotation: {:?}\n\
         Block topic: {:?}\n\
         Scope: {:?}",
        last.annotation,
        annotation,
        last.block,
        scope,
      );
      let mut containing_blocks = containing_blocks.clone();
      let last_mut = containing_blocks.last_mut().unwrap();
      last_mut.annotation = Some(annotation);
      Scope::ContainingBlock {
        container: container.clone(),
        component: component.clone(),
        member: member.clone(),
        containing_blocks,
      }
    }
    _ => panic!(
      "Invariant violation: annotated block node encountered outside a containing block scope"
    ),
  }
}

/// Sets the member in a scope, replacing any existing member.
/// Used for nested headings in documentation where sub-H1 sections
/// should replace the current member rather than nesting further.
pub fn set_member(scope: &Scope, topic: topic::Topic) -> Scope {
  match scope {
    Scope::Global => Scope::Global, // Global scope cannot have members
    Scope::Container { .. } => scope.clone(), // Container needs a component first
    Scope::Component {
      container,
      component,
    } => Scope::Member {
      container: container.clone(),
      component: component.clone(),
      member: topic,
      signature_container: None,
    },
    Scope::Member {
      container,
      component,
      ..
    } => Scope::Member {
      container: container.clone(),
      component: component.clone(),
      member: topic,
      signature_container: None,
    },
    Scope::ContainingBlock {
      container,
      component,
      ..
    } => Scope::Member {
      container: container.clone(),
      component: component.clone(),
      member: topic,
      signature_container: None,
    },
  }
}

/// Sets the signature_container on a Member scope.
/// Panics if the scope is not `Member`.
pub fn set_signature_container(
  scope: &Scope,
  container: topic::Topic,
) -> Scope {
  match scope {
    Scope::Member {
      container: proj,
      component,
      member,
      ..
    } => Scope::Member {
      container: proj.clone(),
      component: component.clone(),
      member: member.clone(),
      signature_container: Some(container),
    },
    _ => panic!(
      "Invariant violation: set_signature_container called on non-Member scope"
    ),
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

/// Kinds of titled topics (topics with a title but not a full declaration)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TitledTopicKind {
  /// Documentation section (H1 becomes component, sub-H1 becomes member)
  DocumentationSection,
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
  Literal,
  SemanticBlock,
  Break,
  Continue,
  Emit,
  InlineAssembly,
  LoopExpression,
  Placeholder,
  Return,
  Revert,
  Try,
  UncheckedBlock,
  Reference,
  MutableReference,
  Signature,
  DocumentationRoot,
  DocumentationHeading,
  DocumentationParagraph,
  DocumentationSentence,
  DocumentationCodeBlock,
  DocumentationInlineCode,
  DocumentationList,
  DocumentationBlockQuote,
  Other,
}

#[derive(Debug, Clone)]
pub enum NamedTopicVisibility {
  Public,
  Private,
  Internal,
  External,
}

/// Represents a reference to a topic, with type information about its source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reference {
  /// A reference from project analysis (solidity analyzer or documentation analyzer).
  ProjectReference {
    reference_topic: topic::Topic,
    sort_key: Option<usize>,
  },
  /// A project reference that is also targeted by one or more comment mentions.
  ProjectReferenceWithMentions {
    reference_topic: topic::Topic,
    mention_topics: Vec<topic::Topic>,
    sort_key: Option<usize>,
  },
  /// A reference from user comments only (not present in source code).
  CommentMention {
    reference_topic: topic::Topic,
    mention_topics: Vec<topic::Topic>,
    sort_key: Option<usize>,
  },
}

impl Reference {
  /// Returns the primary reference topic.
  pub fn reference_topic(&self) -> &topic::Topic {
    match self {
      Reference::ProjectReference {
        reference_topic, ..
      }
      | Reference::ProjectReferenceWithMentions {
        reference_topic, ..
      }
      | Reference::CommentMention {
        reference_topic, ..
      } => reference_topic,
    }
  }

  /// Returns the mention topics, if any.
  pub fn mention_topics(&self) -> Option<&[topic::Topic]> {
    match self {
      Reference::ProjectReference { .. } => None,
      Reference::ProjectReferenceWithMentions { mention_topics, .. }
      | Reference::CommentMention { mention_topics, .. } => {
        Some(mention_topics)
      }
    }
  }

  /// Returns the sort key (source location start) for ordering within a group.
  pub fn sort_key(&self) -> Option<usize> {
    match self {
      Reference::ProjectReference { sort_key, .. }
      | Reference::ProjectReferenceWithMentions { sort_key, .. }
      | Reference::CommentMention { sort_key, .. } => *sort_key,
    }
  }

  /// Creates a new ProjectReference.
  pub fn project_reference(
    reference_topic: topic::Topic,
    sort_key: Option<usize>,
  ) -> Self {
    Reference::ProjectReference {
      reference_topic,
      sort_key,
    }
  }

  /// Creates a new CommentMention.
  pub fn comment_mention(
    reference_topic: topic::Topic,
    mention_topic: topic::Topic,
    sort_key: Option<usize>,
  ) -> Self {
    Reference::CommentMention {
      reference_topic,
      mention_topics: vec![mention_topic],
      sort_key,
    }
  }
}

/// Organizes topics hierarchically by their source scope.
/// For Solidity: scope is a contract, scope_references are contract-level refs, nested_references are function-level refs.
/// For Documentation: scope is a file, scope_references are file-level refs, nested_references are section-level refs.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceContext {
  /// The grouping scope where these references occur (contract for Solidity, file for documentation)
  scope: topic::Topic,
  /// Source location start for sorting groups relative to each other
  sort_key: Option<usize>,
  /// Whether this scope is defined in one of the audit's in-scope files
  is_in_scope: bool,
  /// References at the scope level (inheritance/using-for for Solidity, file-level for documentation)
  scope_references: Vec<Reference>,
  /// References within nested scopes (functions for Solidity, sections for documentation)
  nested_references: Vec<NestedSourceContext>,
}

impl SourceContext {
  pub fn scope(&self) -> &topic::Topic {
    &self.scope
  }

  pub fn sort_key(&self) -> Option<usize> {
    self.sort_key
  }

  pub fn is_in_scope(&self) -> bool {
    self.is_in_scope
  }

  pub fn scope_references(&self) -> &[Reference] {
    &self.scope_references
  }

  pub fn nested_references(&self) -> &[NestedSourceContext] {
    &self.nested_references
  }
}

/// A child element within a nested or annotated block source context.
/// Unifies references and annotated block groups into a single ordered list
/// so that correct linear source order is preserved.
#[derive(Debug, Clone, PartialEq)]
pub enum SourceChild {
  /// A direct reference to a topic at this level.
  Reference(Reference),
  /// A nested annotated block group (control flow body, unchecked, assembly, etc.).
  AnnotatedBlock(AnnotatedBlockSourceContext),
}

impl SourceChild {
  /// Returns the sort key for ordering children relative to each other.
  pub fn sort_key(&self) -> Option<usize> {
    match self {
      SourceChild::Reference(r) => r.sort_key(),
      SourceChild::AnnotatedBlock(a) => a.sort_key(),
    }
  }
}

/// Groups references within an annotated block (control flow body, unchecked, assembly, etc.).
/// Recursive to handle nesting (e.g. if inside for inside unchecked).
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedBlockSourceContext {
  /// The block annotation that groups these references
  annotation: BlockAnnotation,
  /// Source location start for sorting groups relative to each other
  sort_key: Option<usize>,
  /// Ordered children (references and nested annotated blocks) in source order
  children: Vec<SourceChild>,
  /// Whether this If branch has a sibling branch (true body has false body, or vice versa)
  has_sibling_branch: bool,
}

impl AnnotatedBlockSourceContext {
  pub fn annotation(&self) -> &BlockAnnotation {
    &self.annotation
  }

  pub fn sort_key(&self) -> Option<usize> {
    self.sort_key
  }

  pub fn children(&self) -> &[SourceChild] {
    &self.children
  }

  pub fn has_sibling_branch(&self) -> bool {
    self.has_sibling_branch
  }
}

/// Groups references within a nested scope.
/// For Solidity: represents references within a function/modifier.
/// For Documentation: represents references within a section (component).
#[derive(Debug, Clone, PartialEq)]
pub struct NestedSourceContext {
  /// The nested scope containing these references (function for Solidity, section for documentation)
  subscope: topic::Topic,
  /// Source location start for sorting nested groups relative to each other
  sort_key: Option<usize>,
  /// Ordered children (references and annotated block groups) in source order
  children: Vec<SourceChild>,
}

impl NestedSourceContext {
  pub fn subscope(&self) -> &topic::Topic {
    &self.subscope
  }

  pub fn sort_key(&self) -> Option<usize> {
    self.sort_key
  }

  pub fn children(&self) -> &[SourceChild] {
    &self.children
  }
}

/// Ensures a SourceContext exists for the given scope, creating one at the
/// correct sorted position if absent. Does not add any references.
pub fn ensure_context(
  groups: &mut Vec<SourceContext>,
  scope: topic::Topic,
  scope_sort_key: Option<usize>,
  is_in_scope: bool,
) {
  if groups.iter().any(|g| g.scope == scope) {
    return;
  }
  let pos = groups
    .binary_search_by(|g| g.sort_key.cmp(&scope_sort_key))
    .unwrap_or_else(|pos| pos);
  groups.insert(
    pos,
    SourceContext {
      scope,
      sort_key: scope_sort_key,
      is_in_scope,
      scope_references: Vec::new(),
      nested_references: Vec::new(),
    },
  );
}

/// Inserts a reference into a sorted, deduplicated Vec<SourceContext>.
///
/// Finds or creates the appropriate SourceContext (by scope topic) and, if a subscope
/// is provided, the appropriate NestedSourceContext. If an annotation chain is provided,
/// the reference is nested within recursive AnnotatedBlockSourceContext(s) inside the
/// NestedSourceContext.
///
/// Inserts the reference at the correct sorted position. Skips insertion if a reference
/// with the same reference_topic already exists at that level.
pub fn insert_into_context(
  groups: &mut Vec<SourceContext>,
  scope: topic::Topic,
  scope_sort_key: Option<usize>,
  is_in_scope: bool,
  subscope: Option<(topic::Topic, Option<usize>)>,
  annotation_chain: &[BlockAnnotation],
  reference: Reference,
) {
  // Ensure the context exists
  ensure_context(groups, scope.clone(), scope_sort_key, is_in_scope);

  // We know the group exists now — find it
  let group = groups.iter_mut().find(|g| g.scope == scope).unwrap();

  match subscope {
    None => {
      // Insert into scope_references with dedup (no control flow at scope level)
      insert_ref_sorted(&mut group.scope_references, reference);
    }
    Some((subscope_topic, subscope_sort_key)) => {
      // Find or create the NestedSourceContext for this subscope
      if !group
        .nested_references
        .iter()
        .any(|n| n.subscope == subscope_topic)
      {
        let pos = group
          .nested_references
          .binary_search_by(|n| n.sort_key.cmp(&subscope_sort_key))
          .unwrap_or_else(|pos| pos);
        group.nested_references.insert(
          pos,
          NestedSourceContext {
            subscope: subscope_topic.clone(),
            sort_key: subscope_sort_key,
            children: Vec::new(),
          },
        );
      }

      let nested = group
        .nested_references
        .iter_mut()
        .find(|n| n.subscope == subscope_topic)
        .unwrap();

      if annotation_chain.is_empty() {
        insert_child_ref(&mut nested.children, reference);
      } else {
        // Walk the annotation chain, creating/finding groups at each level
        let target_children = find_or_create_annotation_context(
          &mut nested.children,
          annotation_chain,
        );
        insert_child_ref(target_children, reference);
      }
    }
  }
}

/// Walks an annotation chain, creating or finding `AnnotatedBlockSourceContext`s at
/// each level, and returns a mutable reference to the `children` vec at the final level.
fn find_or_create_annotation_context<'a>(
  children: &'a mut Vec<SourceChild>,
  chain: &[BlockAnnotation],
) -> &'a mut Vec<SourceChild> {
  assert!(!chain.is_empty());

  let ann = &chain[0];

  // Find or create the group for this annotation (matched by topic + kind)
  let exists = children.iter().any(|c| {
    matches!(
      c,
      SourceChild::AnnotatedBlock(g)
        if g.annotation.topic == ann.topic && g.annotation.kind == ann.kind
    )
  });

  if !exists {
    // When inserting an If branch, check if the sibling branch already exists
    let has_sibling = matches!(ann.kind, BlockAnnotationKind::If(_))
      && children.iter().any(|c| {
        matches!(
          c,
          SourceChild::AnnotatedBlock(g)
            if g.annotation.topic == ann.topic && g.annotation.kind != ann.kind
        )
      });

    let sort_key = ann.topic.underlying_id().ok().map(|id| id as usize);
    let pos = children
      .binary_search_by(|c| c.sort_key().cmp(&sort_key))
      .unwrap_or_else(|pos| pos);
    children.insert(
      pos,
      SourceChild::AnnotatedBlock(AnnotatedBlockSourceContext {
        annotation: ann.clone(),
        sort_key,
        children: Vec::new(),
        has_sibling_branch: has_sibling,
      }),
    );

    // If we found a sibling, mark the existing sibling too
    if has_sibling {
      for child in children.iter_mut() {
        if let SourceChild::AnnotatedBlock(g) = child {
          if g.annotation.topic == ann.topic && g.annotation.kind != ann.kind {
            g.has_sibling_branch = true;
            break;
          }
        }
      }
    }
  }

  let group = children
    .iter_mut()
    .find_map(|c| match c {
      SourceChild::AnnotatedBlock(g)
        if g.annotation.topic == ann.topic && g.annotation.kind == ann.kind =>
      {
        Some(g)
      }
      _ => None,
    })
    .unwrap();

  if chain.len() == 1 {
    &mut group.children
  } else {
    find_or_create_annotation_context(&mut group.children, &chain[1..])
  }
}

/// Merges `incoming` into `existing` when they share the same reference_topic.
///
/// Merge rules:
/// - ProjectReference + ProjectReference → skip (already present)
/// - CommentMention + CommentMention → merge mention_topics
/// - ProjectReference + CommentMention → promote to ProjectReferenceWithMentions
/// - ProjectReferenceWithMentions + CommentMention → merge mention_topics
/// - CommentMention + ProjectReference → promote to ProjectReferenceWithMentions
fn merge_reference(existing: &mut Reference, incoming: &Reference) {
  let ref_topic = existing.reference_topic().clone();

  match (&mut *existing, incoming) {
    // ProjectReference + ProjectReference → already present, skip
    (
      Reference::ProjectReference { .. },
      Reference::ProjectReference { .. },
    ) => {}

    // ProjectReference + CommentMention → promote to ProjectReferenceWithMentions
    (
      existing_ref @ Reference::ProjectReference { .. },
      Reference::CommentMention { mention_topics, .. },
    ) => {
      let sort_key = existing_ref.sort_key();
      *existing_ref = Reference::ProjectReferenceWithMentions {
        reference_topic: ref_topic,
        mention_topics: mention_topics.clone(),
        sort_key,
      };
    }

    // ProjectReferenceWithMentions + CommentMention → merge mention_topics
    (
      Reference::ProjectReferenceWithMentions {
        mention_topics: existing_mentions,
        ..
      },
      Reference::CommentMention {
        mention_topics: new_mentions,
        ..
      },
    ) => {
      for mt in new_mentions {
        if !existing_mentions.contains(mt) {
          existing_mentions.push(mt.clone());
        }
      }
    }

    // CommentMention + CommentMention → merge mention_topics
    (
      Reference::CommentMention {
        mention_topics: existing_mentions,
        ..
      },
      Reference::CommentMention {
        mention_topics: new_mentions,
        ..
      },
    ) => {
      for mt in new_mentions {
        if !existing_mentions.contains(mt) {
          existing_mentions.push(mt.clone());
        }
      }
    }

    // CommentMention + ProjectReference → promote to ProjectReferenceWithMentions
    (
      existing_ref @ Reference::CommentMention { .. },
      Reference::ProjectReference { .. },
    ) => {
      let sort_key = existing_ref.sort_key();
      let mention_topics = existing_ref.mention_topics().unwrap().to_vec();
      *existing_ref = Reference::ProjectReferenceWithMentions {
        reference_topic: ref_topic,
        mention_topics,
        sort_key,
      };
    }

    // All other combinations with ProjectReferenceWithMentions as the incoming
    // reference shouldn't occur in practice, but handle gracefully
    _ => {}
  }
}

/// Inserts a reference into a sorted Vec<Reference>, merging by reference_topic.
/// Used for SourceContext.scope_references which remains Vec<Reference>.
fn insert_ref_sorted(refs: &mut Vec<Reference>, reference: Reference) {
  if let Some(existing) = refs
    .iter_mut()
    .find(|r| *r.reference_topic() == *reference.reference_topic())
  {
    merge_reference(existing, &reference);
    return;
  }

  let sort_key = reference.sort_key();
  let pos = refs
    .binary_search_by(|r| r.sort_key().cmp(&sort_key))
    .unwrap_or_else(|pos| pos);
  refs.insert(pos, reference);
}

/// Inserts a Reference as a SourceChild into a sorted children list,
/// merging by reference_topic if a matching Reference already exists.
fn insert_child_ref(children: &mut Vec<SourceChild>, reference: Reference) {
  if let Some(existing) = children.iter_mut().find_map(|c| match c {
    SourceChild::Reference(r)
      if *r.reference_topic() == *reference.reference_topic() =>
    {
      Some(r)
    }
    _ => None,
  }) {
    merge_reference(existing, &reference);
    return;
  }

  let sort_key = reference.sort_key();
  let pos = children
    .binary_search_by(|c| c.sort_key().cmp(&sort_key))
    .unwrap_or_else(|pos| pos);
  children.insert(pos, SourceChild::Reference(reference));
}

#[derive(Debug, Clone)]
pub enum TopicMetadata {
  NamedTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: NamedTopicKind,
    name: String,
    visibility: NamedTopicVisibility,
    context: Vec<SourceContext>,
    /// Context derived from recursively traversing all ancestors, descendants, and relatives.
    /// Contains grouped source context showing where all transitive ancestry-related
    /// variables are declared/referenced.
    expanded_context: Vec<SourceContext>,
    /// Context derived from recursively traversing ancestors and descendants only.
    /// Similar to expanded_context but excludes relatives.
    ancestry: Vec<SourceContext>,
    /// Whether this topic has mutations (was previously NamedMutableTopic)
    is_mutable: bool,
    /// The assignment or unary operation nodes that mutate this variable.
    /// Empty for non-mutable topics.
    mutations: Vec<topic::Topic>,
    /// Variables that are true ancestors of this variable
    /// Only populated for variable declarations.
    ancestors: Vec<topic::Topic>,
    /// Variables whose values are derived from this variable.
    /// Only populated for variable declarations.
    descendants: Vec<topic::Topic>,
    /// Variables that are related to this variable:
    /// 1. Appear together in comparison, arithmetic, or bitwise binary operations
    /// 2. Appear as alternatives in conditional (ternary) expressions
    /// 3. Are involved in this variable's assignment (RHS of assignments)
    /// Only populated for variable declarations.
    relatives: Vec<topic::Topic>,
    /// Documentation sections that mention this topic via code identifiers.
    /// Grouped by file (container), with section-level and paragraph-level sub-groups.
    mentions: Vec<SourceContext>,
  },
  UnnamedTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: UnnamedTopicKind,
    context: Vec<SourceContext>,
    mentions: Vec<SourceContext>,
  },
  /// A control flow statement (if/for/while/do-while) with its condition topic.
  ControlFlow {
    topic: topic::Topic,
    scope: Scope,
    kind: ControlFlowStatementKind,
    /// The condition expression topic.
    condition: topic::Topic,
    context: Vec<SourceContext>,
    mentions: Vec<SourceContext>,
  },
  /// A topic with a title (like documentation sections) but not a full declaration
  TitledTopic {
    topic: topic::Topic,
    scope: Scope,
    kind: TitledTopicKind,
    title: String,
    context: Vec<SourceContext>,
    mentions: Vec<SourceContext>,
  },
  /// A comment topic with immutable metadata
  CommentTopic {
    topic: topic::Topic,
    author_id: i64,
    comment_type: String,
    target_topic: topic::Topic,
    created_at: String,
    scope: Scope,
    mentioned_topics: Vec<topic::Topic>,
    context: Vec<SourceContext>,
    mentions: Vec<SourceContext>,
  },
}

impl TopicMetadata {
  pub fn scope(&self) -> &Scope {
    match self {
      TopicMetadata::NamedTopic { scope, .. }
      | TopicMetadata::UnnamedTopic { scope, .. }
      | TopicMetadata::ControlFlow { scope, .. }
      | TopicMetadata::TitledTopic { scope, .. }
      | TopicMetadata::CommentTopic { scope, .. } => scope,
    }
  }

  pub fn name(&self) -> Option<&str> {
    match self {
      TopicMetadata::NamedTopic { name, .. } => Some(name),
      TopicMetadata::TitledTopic { title, .. } => Some(title),
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::CommentTopic { .. } => None,
    }
  }

  pub fn topic(&self) -> &topic::Topic {
    match self {
      TopicMetadata::NamedTopic { topic, .. }
      | TopicMetadata::UnnamedTopic { topic, .. }
      | TopicMetadata::ControlFlow { topic, .. }
      | TopicMetadata::TitledTopic { topic, .. }
      | TopicMetadata::CommentTopic { topic, .. } => topic,
    }
  }

  pub fn context(&self) -> &[SourceContext] {
    match self {
      TopicMetadata::NamedTopic { context, .. }
      | TopicMetadata::UnnamedTopic { context, .. }
      | TopicMetadata::ControlFlow { context, .. }
      | TopicMetadata::TitledTopic { context, .. }
      | TopicMetadata::CommentTopic { context, .. } => context,
    }
  }

  pub fn expanded_context(&self) -> &[SourceContext] {
    match self {
      TopicMetadata::NamedTopic {
        expanded_context, ..
      } => expanded_context,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn ancestry(&self) -> &[SourceContext] {
    match self {
      TopicMetadata::NamedTopic { ancestry, .. } => ancestry,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn ancestors(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { ancestors, .. } => ancestors,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn descendants(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { descendants, .. } => descendants,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn relatives(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { relatives, .. } => relatives,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn mutations(&self) -> &[topic::Topic] {
    match self {
      TopicMetadata::NamedTopic { mutations, .. } => mutations,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => &[],
    }
  }

  pub fn is_mutable(&self) -> bool {
    match self {
      TopicMetadata::NamedTopic { is_mutable, .. } => *is_mutable,
      TopicMetadata::UnnamedTopic { .. }
      | TopicMetadata::ControlFlow { .. }
      | TopicMetadata::TitledTopic { .. }
      | TopicMetadata::CommentTopic { .. } => false,
    }
  }

  pub fn mentions(&self) -> &[SourceContext] {
    match self {
      TopicMetadata::NamedTopic { mentions, .. }
      | TopicMetadata::UnnamedTopic { mentions, .. }
      | TopicMetadata::ControlFlow { mentions, .. }
      | TopicMetadata::TitledTopic { mentions, .. }
      | TopicMetadata::CommentTopic { mentions, .. } => mentions,
    }
  }

  /// Returns the qualified name of the declaration, or None for unnamed topics.
  /// Format: component.member.name, component.name, or name
  /// Uses the declaration names from the scope components, falling back to topic IDs if not found.
  pub fn qualified_name(&self, audit_data: &AuditData) -> Option<String> {
    let name = self.name()?;
    Some(match &self.scope() {
      Scope::Global | Scope::Container { .. } => name.to_string(),
      Scope::Component { component, .. } => {
        let component_name = audit_data
          .topic_metadata
          .get(component)
          .and_then(|d| d.name())
          .unwrap_or_else(|| component.id());
        format!("{}.{}", component_name, name)
      }
      Scope::Member {
        component, member, ..
      }
      | Scope::ContainingBlock {
        component, member, ..
      } => {
        let component_name = audit_data
          .topic_metadata
          .get(component)
          .and_then(|d| d.name())
          .unwrap_or_else(|| component.id());
        let member_name = audit_data
          .topic_metadata
          .get(member)
          .and_then(|d| d.name())
          .unwrap_or_else(|| member.id());
        format!("{}.{}.{}", component_name, member_name, name)
      }
    })
  }
}

pub enum FunctionModProperties {
  FunctionProperties {
    reverts: Vec<RevertInfo>,
    calls: Vec<topic::Topic>,
    mutations: Vec<topic::Topic>,
  },
  ModifierProperties {
    reverts: Vec<RevertInfo>,
    calls: Vec<topic::Topic>,
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

/// Reads "documents.txt" from the project root and returns an ordered list
/// of document file paths. Order matters — documents are parsed in this order
/// to produce deterministic node IDs. New documents should be appended to the
/// end of the file to preserve existing IDs.
pub fn load_document_files(
  project_root: &Path,
) -> Result<Vec<ProjectPath>, String> {
  let doc_file = project_root.join("documents.txt");
  if !doc_file.exists() {
    return Err("documents.txt file not found in project root".to_string());
  }

  let content = std::fs::read_to_string(&doc_file)
    .map_err(|e| format!("Failed to read documents.txt: {}", e))?;

  let mut document_files = Vec::new();
  for line in content.lines() {
    let line = line.trim();
    if !line.is_empty() {
      let project_path = new_project_path(&line.to_string(), project_root);
      document_files.push(project_path);
    }
  }

  Ok(document_files)
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
      context: Vec::new(),
      expanded_context: Vec::new(),
      ancestry: Vec::new(),
      is_mutable: false,
      mutations: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
      mentions: Vec::new(),
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
      context: Vec::new(),
      expanded_context: Vec::new(),
      ancestry: Vec::new(),
      is_mutable: false,
      mutations: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
      mentions: Vec::new(),
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
      context: Vec::new(),
      expanded_context: Vec::new(),
      ancestry: Vec::new(),
      is_mutable: false,
      mutations: Vec::new(),
      ancestors: Vec::new(),
      descendants: Vec::new(),
      relatives: Vec::new(),
      mentions: Vec::new(),
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
    name_index: TopicNameIndex::empty(),
  }
}

pub fn new_data_context() -> DataContext {
  DataContext {
    audits: BTreeMap::new(),
    source_text_cache: HashMap::new(),
    topic_view_cache: HashMap::new(),
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

  /// Cache rendered source text HTML for a topic
  pub fn cache_source_text(
    &mut self,
    audit_id: &str,
    topic_id: &str,
    html: String,
  ) {
    self
      .source_text_cache
      .entry(audit_id.to_string())
      .or_default()
      .insert(topic_id.to_string(), html);
  }

  /// Get cached source text HTML for a topic
  pub fn get_cached_source_text(
    &self,
    audit_id: &str,
    topic_id: &str,
  ) -> Option<&String> {
    self
      .source_text_cache
      .get(audit_id)
      .and_then(|m| m.get(topic_id))
  }

  /// Cache static topic view panels (AST-derived, never invalidated)
  pub fn cache_topic_view(
    &mut self,
    audit_id: &str,
    topic_id: &str,
    cached: CachedTopicView,
  ) {
    self
      .topic_view_cache
      .entry(audit_id.to_string())
      .or_default()
      .insert(topic_id.to_string(), cached);
  }

  /// Get cached static topic view panels
  pub fn get_cached_topic_view(
    &self,
    audit_id: &str,
    topic_id: &str,
  ) -> Option<&CachedTopicView> {
    self
      .topic_view_cache
      .get(audit_id)
      .and_then(|m| m.get(topic_id))
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
