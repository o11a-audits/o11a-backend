# Rem Engine Integration — v2 (value-IR substrate)

> **Status:** design rationale / direction. Supersedes the original
> `docs/plans/engine-integration.md` (no longer on disk), which bet on o11a
> consuming a concrete, serialized Rem HIR and registering AI-annotation
> "sidecars" against it. That bet is dead: Rem pivoted to a trait-over-native-IR
> design with **no concrete HIR and no sidecar producer port**. This document is
> the replacement-of-record.
>
> **Sources this is grounded in:** Rem — `../rem/docs/engine-trait-surface.md`,
> `engine.md`, `engine-disciplines.md`, `engine-characteristics.md`. o11a —
> `crates/o11a-analyze/docs/build-plans/{invariants-step-8,validation-step-10,transitive-effects}.md`,
> `crates/o11a-core/SPEC.md`, `CLAUDE.md`.

---

## 0. TL;DR

1. **Rem owns no IR.** It reasons over a per-function IR that *frontends* expose
   through a trait surface, in a small canonical vocabulary
   (`Statement`/`Terminator`/`Value`/`Predicate<F>`/`Term<F>`), per-function,
   owned-by-value, discarded after. There is nothing durable to consume and no
   sidecar to register. o11a is therefore an **adapter author + reasoning
   client**, not a downstream IR consumer.

2. **o11a's right substrate is a single normalized per-function *value IR*** —
   `FunctionBody`-shaped, `N`-topic-addressable — built once per function by each
   language frontend and walked by three consumers. This is *not* the flat
   "unified source HIR" rejected earlier in design; it is a value/dataflow IR
   scoped by the **pure/impure cut**.

3. **The pure/impure cut is the unify-vs-tag rule.** The pure value layer
   (expressions, operators, arg-binding, returns) *converges* across blockchains
   and is unified into the IR spine. The impure layer (storage vs. accounts,
   `msg.sender` vs. signers, `require` vs. account constraints, `CALL` vs. CPI,
   references vs. resources) *diverges* and is preserved as **chain-tagged
   boundary nodes**, never flattened.

4. **Frontends connect via capability traits, not by exposing raw ASTs.** The
   load-bearing trait obligation is `lower_to_value_ir`. The security pipeline's
   summary projection becomes *derivable from* the IR's boundary nodes.

5. **Invariants become predicates where Rem can check them.** ~14 of o11a's 24
   named `InvariantKind`s map cleanly onto Rem's predicate surface, ~6 are
   partial (recognised-clause-library asks, actionable because the projects are
   siblings), and ~4 are inexpressible — and those 4 coincide almost exactly with
   the kinds `validation-step-10.md` already routes to `Inconclusive`.

---

## 1. What changed in Rem, and why the original plan is void

The original plan's Phase 0 rested on three assumptions. All three are gone:

| Original assumption | Reality under the trait surface |
|---|---|
| HIR-as-data is a stable, serializable artifact o11a can walk and render from | Rem "does not own a concrete IR; it asks frontends to expose theirs through a trait surface." The engine holds opaque `Copy` handles into the frontend's IR and materializes a per-function `FunctionBody<F>` on demand, then discards it. No durable model exists to consume. |
| Sidecar registration admits non-frontend producers (the AI-annotation pattern generalizes) | No first-class non-frontend producer port. Out-of-band data (refinements, effects, trust) enters **only** through the adapter implementing capability traits — `RefinementSource`, `EffectSignatures`, `TrustSurface`. "The engine never parses source text." |
| Effect rows are queryable per-function | Reshaped, not removed: `EffectSignatures::function_effect_signature` is a *capability the adapter implements*; the engine composes inter-procedurally from there. |

Plus a roadmap fact: Rem's reference frontends are **rustc (MIR)** and **Gleam**.
There is **no Rem-supplied Solidity/Solana frontend** and none is phased. The
original plan's "use Rem's Solidity frontend" line does not exist.

**Consequence:** "o11a as a thin IR consumer that deletes its parser/analyzer/
renderer" is impossible. o11a keeps owning a source model. What genuinely
migrates to Rem is narrower and sharper: inter-procedural effect composition and
structured invariant validation.

---

## 2. The substrate: one normalized per-function value IR

o11a is one front end supporting many host languages (Solidity today; Rust/Solana,
Move, others planned). That multi-language goal — *write each pipeline once, run it
on every chain* — is the forcing function for normalization. The question is **what**
to normalize and what to keep native.

The answer is a single materialized, `N`-topic-addressable, normalized **per-function
value IR** (the same shape Rem's `FunctionBody` and rustc's MIR take: statements,
value convergences, calls-as-boundaries). Each frontend builds it from its native
AST. Three consumers walk it:

| Consumer | Side of the cut | What it does with the IR |
|---|---|---|
| **Semantic checker** (new pipeline) | pure / correctness | mechanically walks every value-convergence node; LLM judges purpose/semantics alignment at each junction (type-checker traversal, qualitative verdict) |
| **Rem adapter** | impure / invariant | the IR is already `FunctionBody`-shaped, so the adapter is thin; effects + structured invariants checked mechanically |
| **Security pipeline** (today's 10 steps) | impure / consequences | consumes the summary projection (effects + rendered envelope), *derived from* the IR's boundary nodes |

All three share the same `N`-node identity and the same `P`-family
(`FunctionalSemanticTopic` / `FunctionalPurposeTopic` / `PlacementRationaleTopic`)
attachments at each node. One IR, three readers, shared addressing.

**This is not the rejected flat source HIR.** The earlier design rejected
flattening *divergent impure semantics* (accounts, signers, storage, resources)
into one source shape — that loses the very structure auditors and the LLM need
to see. This IR does the opposite: it unifies only the convergent pure layer and
keeps impure constructs as tagged boundary leaves (§4). It is a refinement of the
layered model, not a reversal of it.

---

## 3. Why a *total* IR, not just a summary projection

An earlier iteration of this design proposed that frontends emit only a **summary
projection** — subject-grained effects + a rendered prompt envelope — because the
existing pipeline consumes a rendered JSON envelope, not the AST. That is
sufficient for the security pipeline alone.

It is **not** sufficient for the semantic checker. The semantic-checker pipeline
walks *every node* and checks, at every convergence of values into an operator/sink,
that the operands' functional purpose and semantics align with the junction's. That
is type checking with the lattice swapped from `type` to `(purpose, semantics)`: the
**traversal is mechanical** (the exhaustive type-checker walk), the **judgment is
qualitative** (LLM). It therefore needs the host's complete *convergence structure*,
node by node — a total view, not a summary.

But "needs the whole function" is **not** "needs the raw host AST." A type checker
walks a normalized typed IR, never raw surface syntax, precisely so it can be
written once over a uniform convergence vocabulary. The semantic checker wants the
same: a normalized convergence IR whose nodes retain source `N`-topic identity so
the `P`-family semantics can be pulled at each junction. That is the total value IR
of §2.

If o11a were Solidity-only, the checker could walk `ASTNode` directly and none of
this would be needed. The IR earns its keep exactly because the checker (and the
Rem adapter) must run on *every* chain.

---

## 4. The pure/impure cut is the unify-vs-tag rule

o11a's correctness theory: **pure computation's only auditable property is
correctness**; impure operations carry a consequence-explosion that produces a large
auditable-property set. Incorrect pure computation feeds impure operations and makes
them vulnerable, but the thing to verify *of the pure computation* is correctness.

This cut doubles as the architectural seam, because the cross-chain divergence lands
**entirely on the impure side**:

| Axis | Solidity (EVM) | Solana (native/Anchor) | Move (Aptos/Sui) | Side |
|---|---|---|---|---|
| Code unit | stateful contract at an address | stateless program (holds no state) | module | impure |
| State location | implicit contract storage | explicit accounts passed into each instruction | global resource store keyed by `(addr, type)` | impure |
| Authority | `msg.sender` (caller) | signer accounts + stored authority pubkey; no `msg.sender` | capability resource / `signer` | impure |
| Authorization mechanism | imperative `require` / modifier | declarative account constraints (`has_one`, `seeds`, `bump`, `owner`, `signer`) | type ability (`key`/`store`) | impure |
| External call | `CALL` / `DELEGATECALL` — reentrant | CPI `invoke_signed` — runtime forbids same-program reentry | direct call, no dynamic dispatch | impure |
| Value | `msg.value` / `payable` | `lamports` field mutation / system CPI | `Coin<T>` resource | impure |
| Conservation | manual (`sum == supply`) | manual | type-enforced (linear resources) | impure |
| **Expressions / operators / arithmetic** | `a + b`, `f(x)`, casts, comparisons | identical shape | identical shape | **pure** |
| **Arg→param binding, returns, assignments** | uniform | uniform | uniform | **pure** |

`AccessGate` means three structurally different things — a `msg.sender == owner`
require, a `signer` + `has_one` constraint on a passed-in account, a capability
argument. A flat source HIR has one way to represent the gate; the other two get
lowered into it lossily, and on Solana that is catastrophic because the account
surface *is* the security model. So: do not flatten impure constructs.

The pure layer, by contrast, converges. A `u64` add is a `u64` add everywhere; a
call binding args to params is the same convergence shape everywhere. Normalizing
the pure layer is **lossless for the property the semantic checker checks**.

**Therefore:**

- **Unify** the pure value spine — expressions, operators, bindings, returns,
  comparisons, the convergence points. This is the semantic checker's whole domain
  and the Rem adapter's dataflow substrate.
- **Tag, do not flatten** the impure constructs — they appear in the IR as
  **boundary nodes** (effect-sink / call / account-write / value-transfer) carrying
  their chain `Kind`. The semantic checker treats a boundary node as a convergence
  *endpoint*: it checks the pure values flowing in (does `value`'s semantics match
  what `slot`/account is meant to hold?) and stops — the consequences of the write
  are the security pipeline's job. The Rem adapter reads the same boundary nodes as
  emissions in Rem's kind-class lattice.

The boundary-node vocabulary is the central design risk (§9): too thin and the
checker can't see the convergence into the sink; too thick and impure idiosyncrasy
leaks back into the spine. Govern it with the same `Kind`/`Tag` discipline Rem uses.

---

## 5. Frontend capability-trait surface

Frontends implement a small set of o11a traits over their **own native AST**; the
pipelines and the single Rem adapter program against the traits. Raw ASTs are never
exposed to the pipelines.

- **`lower_to_value_ir`** *(load-bearing)* — native AST → materialized, normalized,
  `N`-topic-addressable per-function value IR. Pure constructs become uniform spine
  nodes; impure constructs become chain-tagged boundary nodes. Every node retains
  its source `N` topic.
- **`EffectSource`** — per-subject direct effects (reads / writes / calls / reverts /
  events). o11a already computes these for Solidity
  (`solidity/effective_properties.rs`) and the transitive fold is *explicitly
  language-agnostic* (`transitive-effects.md`: "a Rust analyzer … can call
  `effective_properties::compute_transitive_effects`"). Re-found as boundary nodes
  of the IR, the summary projection is a fold over them.
- **`RenderSource`** — produce the LLM prompt projection for a subject (per-chain by
  design: a Solana renderer shows account contexts *as account contexts*; this is a
  feature, not a tax).
- **`ConstraintSource` / `AuthoritySource`** — the chain's authorization surface as
  structured, chain-tagged facts (Solidity modifiers/`require`; Anchor account
  constraints + signer checks; Move capabilities). This is where divergence is
  contained.
- **`RefinementLowering`** — lower an o11a `InvariantTopic` (in its predicate-shaped
  form, §6) into Rem's `Predicate<F>` using the chain's `Kind`/`Tag` vocabulary.

Each is narrow because the unification point (the value IR + topic graph + rendered
envelope + invariant kinds) is already the de-facto contract. Narrowness keeps the
LCD problem bounded and is the argument *for* traits over a fat shared node type.

---

## 6. Invariants as Rem predicates — the LLM structured-output spec

Going forward the LLM is instructed to emit invariants that **are predicates applied
to the subject** wherever possible, so Rem can check them mechanically; the residue
stays prose-only and resolves to `Inconclusive`. Because the projects are siblings,
the partial cases drive concrete additions to Rem's recognised-clause library.

### 6.1 Rem has four predicate modes; the kind selects the mode

"Predicate applied to the subject" is not one shape. Per `engine-trait-surface.md`
(`Predicate<F>` / `Term<F>`, §4.5) and `engine-disciplines.md`:

- **(A) Value refinement** — `Predicate::Relation { lhs, op, rhs }` over `Term`s,
  attached via `RefinementSource::{type_refinements, function_refinements}`,
  propagated by refinement-flow + DBM. Linear ± only. *"P(subject value)."*
- **(B) Emission/protocol ordering** — `ForAllEmissions … e1.position < e2.position`
  over emissions classed into Rem's closed kind-class lattice (`StateWrite`,
  `ExternalCallOut`, `Control`, `Divergence`, …). *"P(subject's execution trace)."*
  CEI is Rem's own worked example (`engine-disciplines.md §8.1`).
- **(C) Tag** — `Predicate::Tag { subject, #Validated }`, a discrete marker stamped
  upstream by a validator/modifier and flowed forward. *"subject was checked before
  it got here."*
- **(D) Residual / forward-existential closure** — `ResidualEmpty { clause }`: every
  acquire has a matching release before the function boundary. Lifecycle pairing,
  one-shot consume.

Hard ceiling (`engine-characteristics.md §3`, structural not aspirational): **no
nonlinear arithmetic** (`x*y` over fields, crypto, modular inverse), **no
cross-structure relations over unbounded collections**, **no multiset/permutation
equality**, **no unbounded inductive measures**, no SMT. Anything needing those
degrades to the sound default evaluator — i.e. is *not* precisely checked.

### 6.2 Mapping of all 25 `InvariantKind`s

(Order per `validation-step-10.md` Phase 1.)

| InvariantKind | Predicate mode | Verdict |
|---|---|---|
| **CheckEffectsInteractions** | B — `state_write.pos < external_call.pos` | ✅ exact (Rem's example) |
| **AccessGate** | B+C — gate-check precedes effect / `#AuthChecked` | ✅ |
| **PauseGate** | B+C — `whenNotPaused` precedes / `#NotPaused` | ✅ |
| **ReentrancyLock** | D — acquire/release residual closure + lock tag | ✅ (monotone lifecycle) |
| **ZeroAddressCheck** | A — `addr ≠ 0` narrowing (canonical scalar refinement) | ✅ |
| **CapBound** | A — `v ≤ CAP` type-refinement at writes (DBM) | ✅ |
| **Monotonic** | A — `new ≥ old` (engine explicitly "reaches monotone") | ✅ |
| **BoundedTolerance** | A — linear relation on caller tolerance | ✅ |
| **FreshnessCheck** | A+B — `now − ts ≤ maxAge` after read emission | ✅ |
| **ReturnValueCheck** | B — `external_call ⇒ check-return follows` | ✅ |
| **CodePresenceCheck** | B — `code.length>0` check precedes delegatecall | ✅ |
| **InitializerGuard** | D — one-shot set on `initialized` (residual) | ✅ |
| **ReplayProtection** | D+B — nonce consume-once (monotone used-flag) | ✅ |
| **BoundedComputation** | `IterationAnnotations` — `@decreases` / `ForAllRange` | ✅ (the loop-bound mechanism *is* this) |
| **PhaseGate** | B+C — phase-check precedes (gate only; not full FSM) | ◻ gate yes, FSM no |
| **RateLimit** | A+B — per-actor `now ≥ last+period` (compound-keyed seq) | ◻ needs per-actor recognised shape |
| **TimelockedAction** | A+B — schedule→action delay relation | ◻ needs schedule/action shape |
| **DualLedger** | B — `write_A ⇒ exists write_B same op` | ◻ needs compound-keyed pairing |
| **InputValidation** | A — len/sentinel bounds ✅; sortedness ✗ | ◻ partial |
| **SignatureValidation** | A — malleability/recovery≠0 ✅; ecrecover soundness ✗ | ◻ partial (crypto out) |
| **OracleManipulation** | TWAP/multi-source = statistical/nonlinear | ❌ → Inconclusive |
| **SumConservation** | `sum(balances)==totalSupply` over unbounded map | ❌ Rem excludes exactly this |
| **EconomicInvariant** | constant-product `x*y=k` = nonlinear | ❌ ("no v1 harness" already) |
| **Other** | unstructured | ❌ |

**~14 clean ✅, ~6 partial ◻, ~4 inexpressible ❌.**

### 6.3 Two findings that make this load-bearing

1. **The inexpressible 4 coincide with o11a's own `Inconclusive` set.**
   `validation-step-10.md` already routes `EconomicInvariant` and `Other` to
   `Inconclusive` ("no v1 harness"); `OracleManipulation` and `SumConservation` are
   the natural extensions. o11a's hand-drawn mechanical/non-mechanical line and Rem's
   structural in-scope/out-of-scope line are the **same line**, derived
   independently. The integration is well-posed: Rem's boundary is not arbitrary
   relative to the taxonomy — it matches it.

2. **The `InvariantKind` should carry its predicate mode.** The LLM does not
   free-author `Predicate<F>` ASTs; it picks a kind, and the kind dictates the
   template shape (A/B/C/D). A `CapBound` emits `Relation(state_var, ≤, cap_term)`;
   a `CheckEffectsInteractions` emits the precedence template; the `RefinementLowering`
   adapter fills the chain-specific kinds/terms. Far more reliable than free-form
   predicate authoring, and the LLM's existing kind-pick step already exists.

### 6.4 Sister-project asks (the 6 partials)

Each partial is a recognised-clause-library gap, not a fundamental limit — and is
exactly the place Rem's predicates can be informed by o11a's invariant needs:

- per-actor **compound-keyed sequence** (RateLimit)
- paired-write **co-occurrence** clause (DualLedger)
- **schedule→action delay** clause (TimelockedAction)
- forward-existential **residual closure wired for the Solidity lock kind**
  (ReentrancyLock at boundary)

The crypto/economic residue (`SignatureValidation` soundness, `OracleManipulation`,
`EconomicInvariant`) is **not** worth asking for — it is outside the bounded-reasoning
posture by construction.

---

## 7. The Rem adapter

- **One adapter conceptually, per-chain `Kind`/`Tag` concretely.** Rem's
  `Predicate<F>` is parameterized on a per-frontend emission/tag vocabulary
  (`EmissionSource::kinds()`, `TagSource::tags()`). A Solidity adapter declares
  `sstore ∈ StateWrite`, `external_call ∈ ExternalCallOut`, `#AuthChecked`; a Solana
  adapter declares `account_write ∈ StateWrite`, `cpi_invoke ∈ ExternalCallOut`,
  `require_signer`, `#SignerChecked`/`#OwnerChecked`. **The per-chain split reappears
  at the adapter no matter what** — which is independent confirmation that fighting
  to unify the *source* layer buys nothing at the Rem boundary.
- **Effects compose on the shared kind-class lattice.** o11a's `effective_*` sets map
  directly: `effective_mutations → StateWrite`, `effective_reverts → Divergence`,
  `calls → ExternalCallOut`, `events → Control`. The lattice is chain-agnostic, so the
  translation has the same shape on every chain even though the leaf kinds differ —
  and o11a already computes the data. Cross-language audits (a Solidity contract
  calling a Rust SDK) compose here, *above* the per-chain vocabulary, without waiting
  on Rem's cross-frontend research.
- **Invariant validation** = `RefinementLowering` emits `Predicate<F>`; Rem returns
  `VerificationDiagnostic` verdicts; the adapter maps them to `ValidationVerdict`
  (`Enforced`/`Absent`/`Partial`/`Inconclusive`). The "engine first, LLM fallback on
  `Inconclusive`/unstructured kinds" model of step 10 survives unchanged; only the
  mechanism changes (capability traits + diagnostic sink, not RPC).

---

## 8. What is superseded from the original bet

Dead and removed from the plan: HIR-as-data consumption; the salsa sidecar-input
tables for the ten artifact kinds; "delete the parser/analyzer/renderer"; dependency
on a Rem-supplied Solidity frontend; opaque keys threaded through Rem sidecars (which
kills the original "sidecar cross-reference invalidation" risk outright — topic IDs
stay entirely o11a-side).

Kept and re-founded: the Solidity/Rust/doc parsers (now feeding `lower_to_value_ir`),
the scope/declaration/reference model, the Topic system, the renderer (now a walker
over the IR's nodes + `P`-family attachments).

---

## 9. Risks

- **Boundary-node vocabulary (the central bet).** Too thin → the semantic checker
  can't see values converging into impure sinks. Too thick → impure idiosyncrasy
  re-enters the spine and the "unify pure / tag impure" discipline breaks. Mitigate
  with Rem's `Kind`/`Tag` discipline applied to boundary nodes.
- **Value-IR design.** Getting the pure spine too thin loses convergence signal the
  semantic checker needs; too thick re-imports per-language form. The pure/impure cut
  is the guide, but the line is judgment-heavy in practice.
- **Predicate translation surface.** The 6 partials require Rem additions; until they
  land, those kinds fall back to LLM `Inconclusive`. Acceptable — it matches the
  existing fallback story.
- **Adapter version coupling.** Owning a Rem adapter couples o11a to Rem's
  `#[non_exhaustive]` canonical-vocabulary versioning. One adapter (not N) keeps the
  blast radius small — another argument against per-language adapters.
- **Three consumers, one IR — schema pressure.** The IR must satisfy the semantic
  checker (node granularity), the Rem adapter (`FunctionBody` shape), and projection
  derivation (boundary folds) simultaneously. Sister-project status means Rem can
  accommodate hard blockers immediately, which de-risks this.

---

## 10. Revised phasing (dependency-ordered)

- **Phase 0 — Re-validate against the trait surface.** Confirm: (a) `Predicate<F>` +
  per-chain `Kind`/`Tag` encode the ✅ and ◻ invariant kinds of §6; (b) the boundary
  consumption is workable from a batch (non-`rustc_driver`) host; (c) the 6 partials'
  recognised-clause asks are scoped with Rem. Raise hard blockers to Rem early
  (siblings — they can accommodate).
- **Phase 1 — Design the value IR (spine + boundary leaves).** The central artifact.
  Solidity-first, with the Solana boundary shape sketched so the spine is not
  accidentally Solidity-only. Define the boundary-node vocabulary and `N`-topic
  retention.
- **Phase 2 — Repoint existing pipelines onto the IR.** `lower_to_value_ir` for
  Solidity; renderer → IR walker; re-derive the summary projection from boundary
  nodes; re-tune the security pipeline prompts behind a flag with the existing
  `Inconclusive`-rate parity gate. No Rem yet — de-risk the IR independently.
- **Phase 3 — Semantic-checker pipeline.** The type-checker-style walk over the IR's
  convergence nodes with per-junction LLM purpose/semantics-alignment judgment. Pure
  side; consumes the IR directly; emits a new finding kind.
- **Phase 4 — Rem adapter (effects).** `FrontendDatabase` + `DiagnosticSink` +
  `EmissionSource` + `TagSource`, then `EffectSignatures`. Migrate effect composition
  to engine queries; keep `effective_properties` as the fallback/oracle during
  bring-up.
- **Phase 5 — `RefinementLowering` + engine-backed step 10.** Predicate-shaped
  invariants → `Predicate<F>` → `ValidationVerdict`. LLM fallback for ❌/◻-pending.
- **Phase 6 — Second frontend (Rust/Solana) into the same IR.** Cross-language audit
  drops out of the unified pure spine + shared kind-class lattice rather than waiting
  on Rem's cross-frontend composition.

---

## 11. Open questions

- Should the value IR be materialized eagerly (chosen here, matching o11a's batch
  posture and three-consumer reuse) or walked lazily via a visitor trait (Rem's
  on-demand posture)? Eager is assumed; revisit if memory pressure on large audits
  bites.
- Does the semantic checker emit a new Topic family, or reuse the `P`-family with a
  verdict, paralleling step 10's `ValidationTopic`? Likely the latter for addressing
  uniformity.
- Exact boundary-node taxonomy: is "account write," "CPI," "value transfer," "storage
  write," "event," "revert" the right closed set, mapped onto Rem's kind-class
  lattice, with chain-tagged `Kind` leaves? This is Phase 1's core deliverable.
