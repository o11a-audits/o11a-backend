//! # o11a unified intermediate representation (IR) — scaffold
//!
//! This module defines the **unified value IR** that both the Solidity and the
//! Rust frontends will lower into. It is the single normalized
//! substrate three consumers walk:
//!
//! 1. **Semantic checker** (pure / correctness side) — a type-checker-style walk
//!    over every value-convergence node, with per-junction LLM judgment of whether
//!    operand purpose/semantics align.
//! 2. **Rem adapter** (impure / invariant side) — the IR is `FunctionBody`-shaped,
//!    so the adapter is thin: effects map onto Rem's kind-class lattice, structured
//!    invariants lower into `Predicate<F>`.
//! 3. **Security pipeline** (impure / consequences side) — consumes a *summary
//!    projection* (effects + rendered envelope) derived from this IR's boundary
//!    nodes. Today that projection is produced directly by the two-pass analyzer
//!    (`FunctionModProperties`, `topic_metadata`, …); after migration it is folded
//!    out of the IR.
//!
//! ## The pure/impure cut is the unify-vs-tag rule
//!
//! The pure value layer (expressions, operators, arg→param binding, returns,
//! comparisons) *converges* across blockchains and is **unified** into the spine
//! here. The impure layer (storage vs. accounts, `msg.sender` vs. signers,
//! `require` vs. account constraints, `CALL` vs. CPI, references vs. resources)
//! *diverges* and is preserved as **chain-tagged leaves** — never flattened into
//! the spine. See `docs/plans/engine-integration-v2.md`.
//!
//! ## What is *pre-IR*
//!
//! The Solidity AST (`crate::solidity::ast::ASTNode`) and its **transform phase**
//! (`o11a-analyze/src/solidity/transform.rs`) stay upstream of this IR. The
//! transform phase already: wraps each call argument in an `Argument` node that
//! links value → parameter declaration (the arg→param convergence this IR keeps),
//! remaps interface references to their single implementation, and stamps
//! `call_purity` (Pure/NonPure). The IR builder therefore consumes a
//! transform-normalized AST and inherits those hooks.
//!
//! ## Node identity
//!
//! Every IR node is addressed by the source [`Topic`] it lowered from (`N`-prefix)
//! via [`IrId`]. This is what lets the `P`-family (functional semantics / purpose /
//! placement) and the security model (`A`-family) attach to IR nodes through the
//! existing `topic_metadata` / `declaration_semantics` / `subject_*` maps — the IR
//! holds *structure*, not the qualitative annotations, which stay keyed by topic.
//!
//! ## Status
//!
//! **Scaffold only.** These are type definitions. Nothing constructs them yet, no
//! `AuditData` field holds them, and the analyzer is unchanged. The eventual home
//! is a field on `AuditData` (e.g. `ir: BTreeMap<ProjectPath, Module>`), produced
//! by the analyzer and consumed by the three pipelines above.

use super::ProjectPath;
use super::topic::Topic;
use serde::{Deserialize, Serialize};

// ============================================================================
// Identity
// ============================================================================

/// The stable address of an IR node: the source `Topic` (`N`-prefixed) it
/// lowered from. Qualitative annotations attach to IR nodes through this id.
///
/// DESIGN TENSION (node identity / synthetic nodes): source AST nodes already
/// own `N`-topics, but the IR introduces *synthetic* nodes — desugared compound
/// assignments, implicit conversions, the transform phase's own `Argument` /
/// `LoopExpression` synthetics. Synthetic nodes need ids that (a) never collide
/// with source `N`-topics and (b) stay stable across re-runs so comments and
/// approvals keep sticking. `ids.rs` owns the `N` counter; the IR needs either a
/// reserved synthetic-`N` range or a parallel `reseed_*`-style allocator.
#[derive(
  Debug,
  Clone,
  Copy,
  PartialEq,
  Eq,
  Hash,
  PartialOrd,
  Ord,
  Serialize,
  Deserialize,
)]
#[serde(transparent)]
pub struct IrId(pub Topic);

/// Which frontend lowered this module. The top-level chain tag; finer-grained
/// divergence is carried on individual leaf variants below.
///
/// DESIGN TENSION (dual-target now vs. later): the Rust frontend is a stub today
/// (only `ASTNode::SourceFile`), so every Rust/Solana-specific shape here is
/// guided by a *model* of those chains, not a real frontend. The polyglot model
/// is already committed in `semantic-resolution-graph.md`, but premature
/// abstraction is a real risk until a second frontend exercises these seams.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceLang {
  Solidity,
  Rust,
}

// ============================================================================
// Top-level structure
// ============================================================================

/// One compilation unit (a source file), keyed elsewhere by [`ProjectPath`].
///
/// DESIGN TENSION (IR scope — structure vs. body): this `Module`/`Item` layer
/// re-represents declaration structure that `TopicMetadata` + `Scope` +
/// `FunctionModProperties` already encode. The migration question is whether
/// those become *derived from* the IR (single source of truth) or *coexist*
/// (dual source of truth). The v2 plan says the projection is derived from the
/// IR — implying `topic_metadata`/`function_properties` eventually fold out of
/// `Module`. The scaffold cannot settle this without migrating; for now it models
/// the full module so the IR can be the analyzer's primary output.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
  pub path: ProjectPath,
  pub lang: SourceLang,
  pub items: Vec<Item>,
}

/// A top-level or nested declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Item {
  Container(Container),
  Function(Function),
  /// Contract-owned persistent state (Solidity state variable).
  ///
  /// DESIGN TENSION (state model divergence): this models Solidity's *implicit
  /// contract storage*. Solana programs are stateless — state lives in *accounts
  /// passed into each instruction*, not in a per-program storage list. A Solana
  /// frontend would not emit `StorageVar`; it would emit an account-context leaf
  /// that has no Solidity analog. This is the single largest cross-chain
  /// divergence and lands entirely on the impure side, as expected.
  Storage(StorageVar),
  Struct(StructDecl),
  Enum(EnumDecl),
  Event(EventDecl),
  Error(ErrorDecl),
  Constant(ConstantDecl),
  TypeAlias(TypeAliasDecl),
}

/// A code container: a Solidity contract/library/interface, or a Rust module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Container {
  pub id: IrId,
  pub name: String,
  pub kind: ContainerKind,
  /// Base contracts / interfaces (Solidity inheritance) or supertraits.
  pub bases: Vec<Topic>,
  pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ContainerKind {
  Contract,
  AbstractContract,
  Library,
  Interface,
  /// Rust module (and the eventual home of a `SolanaProgram` discriminant —
  /// a stateless program container with an account-context surface).
  Module,
}

// ============================================================================
// Declarations
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
  pub id: IrId,
  pub name: String,
  pub kind: FunctionKind,
  pub visibility: Visibility,
  pub mutability: StateMutability,
  pub params: Vec<Param>,
  pub returns: Vec<Param>,
  /// The chain-tagged authorization surface attached to this function.
  pub guards: Vec<AuthorityGuard>,
  /// `None` for unimplemented declarations (interface signatures, abstract).
  ///
  /// DESIGN TENSION (modifier expansion): a Solidity function's *effective* body
  /// is its modifier-expanded body (guards run, then `_` is replaced by the body).
  /// Storing the raw body with a [`StmtKind::Placeholder`] is faithful to source
  /// (best for rendering + node identity); storing the inlined/expanded body is
  /// best for the semantic checker and Rem (they see the guard before the body).
  /// The scaffold keeps the body raw and references modifiers via `guards`;
  /// whether a later expansion pass materializes the inlined view is unresolved.
  pub body: Option<Body>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FunctionKind {
  Function,
  Constructor,
  /// Solidity modifier — a body with a [`StmtKind::Placeholder`]. No Rust/Solana
  /// analog; on those chains the authorization surface is the account-constraint
  /// preamble carried in `guards`, not a wrapping body.
  Modifier,
  Fallback,
  Receive,
  /// Free (file-level) function.
  Free,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Param {
  pub id: IrId,
  pub name: String,
  /// `None` until a type pass populates it. See [`IrType`].
  pub ty: Option<IrType>,
}

/// A contract-owned state variable. See the divergence note on [`Item::Storage`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StorageVar {
  pub id: IrId,
  pub name: String,
  pub ty: Option<IrType>,
  pub mutability: VarMutability,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructDecl {
  pub id: IrId,
  pub name: String,
  pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
  pub id: IrId,
  pub name: String,
  pub ty: Option<IrType>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumDecl {
  pub id: IrId,
  pub name: String,
  /// `(member id, member name)`.
  pub variants: Vec<(IrId, String)>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EventDecl {
  pub id: IrId,
  pub name: String,
  pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ErrorDecl {
  pub id: IrId,
  pub name: String,
  pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstantDecl {
  pub id: IrId,
  pub name: String,
  pub ty: Option<IrType>,
  pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeAliasDecl {
  pub id: IrId,
  pub name: String,
  pub underlying: Option<IrType>,
}

// ============================================================================
// Authority / constraint surface (the divergence leaf)
// ============================================================================

/// The chain-tagged authorization surface of a function — what the v2 plan calls
/// the `ConstraintSource` / `AuthoritySource` seam. This is where the dominant
/// per-chain security divergence lives, intact, instead of being flattened.
///
/// DESIGN TENSION (the central boundary-vocabulary risk): `AccessGate` means three
/// structurally different things — a `msg.sender == owner` require (Solidity, an
/// inline check or modifier), a `signer` + `has_one` constraint on a passed-in
/// account (Solana, a declarative attribute), a capability argument (Move). Model
/// this leaf too thin and the semantic checker / Rem can't see the gate; too thick
/// and impure idiosyncrasy leaks back into the spine. Only the Solidity variants
/// are modeled now; the Solana account-constraint variant is the first real test
/// of whether the seam is right.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AuthorityGuard {
  /// A Solidity modifier invocation on the function (`onlyOwner`, `nonReentrant`).
  Modifier {
    invocation: IrId,
    /// The modifier declaration, if resolved.
    modifier: Option<Topic>,
    args: Vec<Expr>,
  },
  // Future: SolanaAccountConstraint { account, kind: AccountConstraintKind, ... },
  //         MoveCapability { resource, ... }
}

// ============================================================================
// Function body — the value IR
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Body {
  pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Stmt {
  pub id: IrId,
  pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StmtKind {
  /// A semantic block: statements the source grouped (by blank lines) with an
  /// optional leading doc comment.
  ///
  /// DESIGN TENSION (rendering fidelity vs. dataflow): o11a's `SemanticBlock`
  /// grouping is load-bearing for rendering and doc-attachment, but a flat
  /// statement list is cleaner for dataflow. Keeping `Block` nesting preserves
  /// fidelity at the cost of the checker/adapter having to flatten on walk.
  Block {
    doc: Option<String>,
    stmts: Vec<Stmt>,
  },
  /// Local variable declaration(s) with an optional initializer. A tuple-LHS
  /// declaration binds several places to one value (a convergence).
  Let {
    places: Vec<Place>,
    value: Option<Expr>,
  },
  /// Assignment: `place op= value`. The assignment is a **convergence** of
  /// `value` into `place`; if `place`'s root is `Storage`, this is the
  /// `StateWrite` boundary (see the boundary-representation tension on [`Place`]).
  Assign {
    place: Place,
    op: AssignOp,
    value: Expr,
  },
  /// An expression evaluated for effect (a call, an `unary++`, etc.).
  Expr(Expr),
  If {
    cond: Expr,
    then_body: Vec<Stmt>,
    else_body: Vec<Stmt>,
  },
  Loop {
    kind: LoopKind,
    /// `for`-init; `None` for while/do-while.
    init: Option<Box<Stmt>>,
    cond: Option<Expr>,
    /// `for`-step; `None` otherwise.
    step: Option<Box<Stmt>>,
    body: Vec<Stmt>,
  },
  Return {
    /// Each return value converges with its return slot.
    values: Vec<Expr>,
  },
  Break,
  Continue,
  /// A statement-level effect (emit / revert). See [`Effect`].
  Effect(Effect),
  /// Solidity modifier placeholder `_`. No-op on other chains.
  Placeholder,
  /// `unchecked { … }` block — arithmetic does not revert on overflow.
  Unchecked {
    stmts: Vec<Stmt>,
  },
  /// Yul / inline assembly — kept opaque. The semantic checker treats it as an
  /// opaque sink/source; its effects are not modeled in v1.
  InlineAssembly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LoopKind {
  For,
  While,
  DoWhile,
}

// ============================================================================
// Expressions — the pure spine
// ============================================================================

/// A value-producing node. Carries its source id, an optional resolved type, and
/// a purity classification. The semantic checker walks these uniformly; interior
/// nodes (those with ≥2 operands) are the **convergence points** it judges.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Expr {
  pub id: IrId,
  /// `None` until a type pass runs.
  pub ty: Option<IrType>,
  /// Whether evaluating this expression is free of impure effects.
  ///
  /// DESIGN TENSION (two notions of purity): the transform phase's `call_purity`
  /// is *declared* purity (from the callee's `stateMutability`), per call site.
  /// What the value IR wants is *transitive effective* purity — an expr is impure
  /// iff any sub-expression reaches a boundary. These differ (a `view` function
  /// that reads storage is "pure" by declaration but reads state). Who computes
  /// this — the IR builder, or a later pass mirroring `effective_properties`? And
  /// which notion does the security pipeline's "non-pure subject" use?
  pub purity: Purity,
  pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprKind {
  Literal(Literal),
  /// Reading a place. A read of a `Storage`-rooted place is the `StateRead`
  /// boundary.
  Read(Place),
  /// Operator convergence of two operands.
  Binary {
    op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
  },
  Unary {
    op: UnOp,
    operand: Box<Expr>,
  },
  /// Ternary `cond ? then : els` — a selection convergence.
  Conditional {
    cond: Box<Expr>,
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
  },
  Call(Call),
  /// Explicit type conversion `T(x)`.
  Cast {
    ty: IrType,
    operand: Box<Expr>,
  },
  /// Struct / array literal construction — each arg converges with a field slot.
  Construct {
    ty: Option<IrType>,
    /// `(field topic if known, value)` — arg→field convergence.
    args: Vec<(Option<Topic>, Expr)>,
  },
  Tuple(Vec<Expr>),
  /// A construct the builder could not lower; kept so the walk is total. Carries
  /// the source node type for diagnostics. The checker treats it as opaque.
  Unsupported {
    node_type: String,
  },
}

// ============================================================================
// Places (lvalues)
// ============================================================================

/// A storage/memory location: a root plus a projection path, mirroring Rem's
/// `PlacePath`. Reads and writes resolve through this so the security projection
/// can name the touched declaration and Rem can key per-cell refinement state.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Place {
  pub id: IrId,
  pub root: PlaceRoot,
  pub projections: Vec<Projection>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PlaceRoot {
  Local(Topic),
  Param(Topic),
  /// Contract-owned state. A write here is the `StateWrite` boundary; a read is
  /// `StateRead`. See the boundary-representation tension below.
  Storage(Topic),
  /// A language builtin (`msg.sender`, `block.timestamp`, `this`). On Solana the
  /// analogous roots are signer/clock/account-meta — a divergent leaf set.
  Builtin(String),
  /// Reference present in source but unresolved by the frontend.
  Unresolved,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Projection {
  /// `.field` — the member declaration, if resolved.
  Field { field: Option<Topic>, name: String },
  /// `[index]` — array/mapping access. For a mapping this is a per-key projection;
  /// see the unbounded-collection note below.
  ///
  /// DESIGN TENSION (unbounded collections): `balances[user]` is a clean place,
  /// but properties *over the whole mapping* (e.g. `SumConservation`:
  /// `sum(balances) == totalSupply`) are exactly what Rem cannot check (aggregate
  /// over an unbounded collection). The IR represents the access fine; the
  /// divergence shows up downstream in what is mechanically checkable.
  Index { index: Box<Expr> },
}

// ============================================================================
// Calls
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Call {
  pub callee: Callee,
  /// Arg→param convergences, inherited from the transform phase's `Argument`
  /// wrapper. Each binding pairs a value with the parameter slot it flows into.
  pub args: Vec<ArgBinding>,
  pub mechanism: CallMechanism,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArgBinding {
  /// The parameter declaration this argument binds to, if resolved.
  pub param: Option<Topic>,
  pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Callee {
  /// A directly-named function/modifier (`foo(...)`).
  Direct { target: Option<Topic> },
  /// A member call (`obj.foo(...)`) — the receiver is itself a value.
  Member {
    receiver: Box<Expr>,
    target: Option<Topic>,
  },
  /// A call through a function value / selector the frontend could not resolve.
  Dynamic { value: Box<Expr> },
}

/// How the call dispatches. The boundary-bearing mechanisms (`External`,
/// `DelegateCall`, `StaticCall`, value-bearing variants) are the impure leaves;
/// `Internal` to a pure callee stays on the pure spine.
///
/// DESIGN TENSION (boundary representation): boundaries are represented two ways
/// in this scaffold — impure **calls** carry a `mechanism` (here), statement-level
/// **emit/revert** are explicit [`Effect`] nodes, and **state reads/writes** are
/// *derived* from a `Storage` [`PlaceRoot`] rather than being their own nodes.
/// That asymmetry (some boundaries explicit, some classified) keeps the value
/// spine clean for the checker but means "enumerate all boundaries" is a
/// classification pass, not a node-kind filter. The alternative — every effect as
/// an explicit boundary node — is uniform for the Rem adapter but redundant for
/// the checker (a store-to-storage is also a plain assignment convergence). Unresolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallMechanism {
  /// Internal/library call within the same execution context.
  Internal,
  /// External message call (`addr.foo()`), reentrancy-bearing.
  External,
  /// `delegatecall` — executes callee code in the caller's storage context.
  DelegateCall,
  /// `staticcall` — read-only external call.
  StaticCall,
  /// Library-attached method (`x.mulDiv(...)` after `using … for`).
  LibraryAttached,
  /// `new C(...)` contract creation.
  ConstructorNew,
}

// ============================================================================
// Statement-level effects (boundary nodes)
// ============================================================================

/// A statement that is primarily an effect rather than a value: emit / revert.
/// (External calls live on [`Call`]; state writes are derived from `Storage`
/// places — see the boundary-representation tension on [`CallMechanism`].)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Effect {
  pub id: IrId,
  /// The shared, chain-agnostic effect lattice (maps onto Rem's kind-class
  /// lattice). What composes cross-chain.
  pub class: EffectClass,
  pub kind: EffectKind,
  /// Values flowing into the effect (event args, revert error args).
  pub operands: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EffectKind {
  Emit { event: Option<Topic> },
  Revert { error: Option<Topic> },
}

/// The chain-agnostic effect classification. Mirrors Rem's closed kind-class
/// lattice (`StateRead`/`StateWrite`/`ExternalCallOut`/…) so o11a's effects
/// translate to Rem emissions with the same shape on every chain, even though the
/// concrete boundary leaves differ. This is the unification that pays off.
///
/// DESIGN TENSION (lattice openness): Rem's kind-class set is *closed and
/// engine-versioned*. If o11a's `EffectClass` is the producer side of that
/// mapping, it should track Rem's set — coupling o11a to Rem's versioning. Keeping
/// it independent risks drift at the adapter boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EffectClass {
  StateRead,
  StateWrite,
  ExternalCall,
  ValueFlow,
  EventEmit,
  Divergence,
  Resource,
  Control,
}

// ============================================================================
// Operators
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Pow,
  Shl,
  Shr,
  BitAnd,
  BitOr,
  BitXor,
  Lt,
  Gt,
  Le,
  Ge,
  Eq,
  Ne,
  And,
  Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnOp {
  Not,
  BitNot,
  Neg,
  Pos,
  PreInc,
  PreDec,
  PostInc,
  PostDec,
  Delete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignOp {
  Assign,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  ModAssign,
  ShlAssign,
  ShrAssign,
  BitAndAssign,
  BitOrAssign,
  BitXorAssign,
}

// ============================================================================
// Literals
// ============================================================================

/// DESIGN TENSION (numeric width): Solidity numerics are up to 256-bit and do not
/// fit native Rust integers, so numbers are kept as their source string here.
/// Solana/Rust numerics top out at `u128`. A shared literal type that is faithful
/// to both either keeps strings (lossless, awkward to compute on) or carries a
/// big-integer (heavier). The scaffold keeps strings; a value-range pass would
/// reinterpret against [`IrType`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
  Number(String),
  Bool(bool),
  Str(String),
  HexBytes(String),
  Address(String),
  Unit,
}

// ============================================================================
// Types
// ============================================================================

/// A normalized, chain-neutral type. Maps from `crate::domain::SolidityType` on
/// the Solidity side.
///
/// DESIGN TENSION (type-system duplication & divergence): this duplicates
/// `SolidityType` (Solidity-named) until migration — two type representations in
/// flight. Worse, the type *lattice itself* partly diverges: `Map` is a Solidity
/// storage construct with no Solana analog (Solana uses account collections), and
/// `Address{payable}` is EVM-specific (Solana addresses are pubkeys with no
/// payable flag). How chain-neutral can `IrType` honestly be before it, too, needs
/// tagged leaves?
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrType {
  Bool,
  Int {
    signed: bool,
    bits: u16,
  },
  Bytes {
    fixed: Option<u16>,
  },
  Address {
    payable: bool,
  },
  Str,
  Array {
    elem: Box<IrType>,
    len: Option<u64>,
  },
  Map {
    key: Box<IrType>,
    value: Box<IrType>,
  },
  Tuple(Vec<IrType>),
  /// A user-defined type (struct/enum/contract) addressed by its declaration.
  Named(Topic),
  Function {
    params: Vec<IrType>,
    returns: Vec<IrType>,
  },
  Unit,
  /// Type the frontend could not normalize.
  Opaque,
}

// ============================================================================
// Classifications
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Purity {
  Pure,
  Impure,
  /// Not yet classified.
  Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
  Public,
  External,
  Internal,
  Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateMutability {
  Pure,
  View,
  Payable,
  NonPayable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarMutability {
  Mutable,
  Immutable,
  Constant,
}

// ============================================================================
// Convergence (semantic-checker concept)
// ============================================================================

/// Classifies the kinds of **value convergence** the semantic checker visits as
/// it walks the value IR. Not stored on nodes — the checker derives a convergence
/// of a given kind from the node shape (e.g. an `ExprKind::Binary` is an
/// `Operator` convergence of its two operands; a [`Call`] yields one `ArgToParam`
/// convergence per [`ArgBinding`]). Listed here as the spec for that walk.
///
/// The checker's traversal is mechanical (this enumeration is total over the IR);
/// the per-convergence judgment ("do these operands' purpose/semantics align?") is
/// qualitative (LLM). This is type checking with the lattice swapped from `type`
/// to `(purpose, semantics)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConvergenceKind {
  /// Two operands meeting under a binary operator.
  Operator,
  /// An argument value flowing into a parameter slot.
  ArgToParam,
  /// A value flowing into an assignment target / let binding.
  Assignment,
  /// A value flowing into a return slot.
  Return,
  /// The two arms of a conditional meeting at the selection point.
  ConditionalSelect,
  /// A value flowing into a struct/array field slot during construction.
  Construct,
}
