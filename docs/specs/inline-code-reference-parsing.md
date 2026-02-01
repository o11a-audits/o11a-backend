# Inline Code and Code Block Reference Parsing

## Overview

Enhance the documentation parser to extract and resolve multiple identifier references from inline code and code blocks, rather than treating them as single atomic references. Additionally, provide syntax highlighting for keywords and operators to support both Solidity and Rust contracts.

## Current Behavior

Currently, `InlineCode` nodes attempt to match the **entire code value** against a single declaration:

```rust
// Current: only matches if entire value equals a declaration name
let referenced_declaration = find_declaration_by_name(audit_data, &code.value)
```

For example, `` `participationState[addr] = ParticipationStatus.STARTED` `` would only match if there's a declaration literally named "participationState[addr] = ParticipationStatus.STARTED" (which there won't be).

## Proposed Behavior

Parse inline code and code blocks to extract individual tokens, then:
1. Resolve identifiers against known declarations
2. Classify keywords for syntax highlighting
3. Classify operators for syntax highlighting
4. Preserve other text (punctuation, literals, whitespace)

### Example

Input: `` `if (participationState[addr] == ParticipationStatus.STARTED) return true;` ``

Output structure:
```
InlineCode {
  node_id: 123,
  value: "if (participationState[addr] == ParticipationStatus.STARTED) return true;",
  children: [
    CodeKeyword { value: "if" },
    CodeText { value: " (" },
    CodeIdentifier { value: "participationState", referenced_topic: Some(Topic("N-456")) },
    CodeText { value: "[" },
    CodeIdentifier { value: "addr", referenced_topic: Some(Topic("N-789")) },
    CodeText { value: "] " },
    CodeOperator { value: "==" },
    CodeText { value: " " },
    CodeIdentifier { value: "ParticipationStatus", referenced_topic: Some(Topic("N-101")) },
    CodeText { value: "." },
    CodeIdentifier { value: "STARTED", referenced_topic: Some(Topic("N-102")) },
    CodeText { value: ") " },
    CodeKeyword { value: "return" },
    CodeText { value: " " },
    CodeKeyword { value: "true" },
    CodeText { value: ";" },
  ]
}
```

## New Node Types

### CodeKeyword

Represents a language keyword (Solidity or Rust).

```rust
CodeKeyword {
  node_id: i32,
  value: String,  // The keyword text (e.g., "if", "return", "pub", "fn")
}
```

### CodeOperator

Represents an operator.

```rust
CodeOperator {
  node_id: i32,
  value: String,  // The operator text (e.g., "==", "+=", "&&", "=>")
}
```

### CodeIdentifier

Represents a potential identifier within code that may reference a declaration.

```rust
CodeIdentifier {
  node_id: i32,
  value: String,                           // The identifier text (e.g., "participationState")
  referenced_topic: Option<topic::Topic>,  // Resolved topic if found
  class: String,                           // CSS class for syntax highlighting (e.g., "contract", "function", "mutable-state-variable")
}
```

The `class` field is populated at parse time based on the topic metadata kind, avoiding the need for later lookup during formatting.

### CodeText

Represents other text within code (punctuation, whitespace, literals, etc.).

```rust
CodeText {
  node_id: i32,
  value: String,  // The text (e.g., "[", ")", " ", "\"hello\"")
}
```

## Modified Node Types

### InlineCode

Change from a leaf node to a container node:

```rust
// Before
InlineCode {
  node_id: i32,
  value: String,
  referenced_declaration: Option<topic::Topic>,
}

// After
InlineCode {
  node_id: i32,
  value: String,           // Original full text (preserved for display fallback)
  children: Vec<DocumentationNode>,  // CodeIdentifier and CodeText nodes
}
```

### CodeBlock

Add children for parsed identifiers:

```rust
// Before
CodeBlock {
  node_id: i32,
  lang: Option<String>,
  value: String,
}

// After
CodeBlock {
  node_id: i32,
  lang: Option<String>,
  value: String,           // Original full text (preserved)
  children: Vec<DocumentationNode>,  // CodeIdentifier and CodeText nodes
}
```

## Tokenization Strategy

### Multi-pass Tokenizer

Use a tokenizer that identifies keywords, operators, and identifiers:

1. **Operator pattern**: Match multi-character operators first, then single-character
2. **Identifier pattern**: `[a-zA-Z_][a-zA-Z0-9_]*`
3. **Keyword check**: If identifier matches a keyword, emit `CodeKeyword`
4. **Declaration check**: If identifier matches a declaration, emit `CodeIdentifier` with topic
5. **Everything else**: Collected as `CodeText`

### Tokenization Rules

1. Scan the code string character by character
2. **Check for operators first** (longest match): Look for multi-character operators like `==`, `!=`, `+=`, `=>`, `::`, then single-character operators
3. When an identifier start character is found (`[a-zA-Z_]`), collect the full identifier
4. **Check if identifier is a keyword**: If yes, create `CodeKeyword`
5. **Otherwise, look up in `audit_data.topic_metadata`**: If found, create `CodeIdentifier` with the resolved topic
6. If not found as keyword or declaration, create `CodeIdentifier` with `referenced_topic: None`
7. Non-identifier, non-operator characters are collected into `CodeText` nodes
8. Adjacent `CodeText` content can be merged for efficiency

### Solidity Keywords

```
// Control flow
if, else, for, while, do, break, continue, return, try, catch, revert, throw

// Function/modifier
function, modifier, constructor, fallback, receive, returns

// Visibility
public, private, internal, external

// Mutability
pure, view, payable, constant, immutable

// Storage
memory, storage, calldata

// Contract structure
contract, interface, library, abstract, is, using, import, pragma

// Types
mapping, struct, enum, event, error, type

// Literals/values
true, false, wei, gwei, ether, seconds, minutes, hours, days, weeks, years

// Other
new, delete, emit, indexed, anonymous, virtual, override, assembly
```

### Rust Keywords

```
// Control flow
if, else, for, while, loop, break, continue, return, match

// Function/module
fn, mod, use, pub, crate, self, super, impl, trait, where

// Types
struct, enum, type, const, static, let, mut, ref, move

// Async
async, await

// Other
as, dyn, extern, in, unsafe, macro_rules
```

### Combined Keyword Set

The tokenizer will use a combined set of Solidity and Rust keywords. If an identifier matches any keyword from either language, it becomes a `CodeKeyword`.

### Operators (Solidity + Rust)

```
// Multi-character (check these first, longest match)
==, !=, <=, >=, &&, ||, <<, >>, +=, -=, *=, /=, %=, &=, |=, ^=,
<<=, >>=, ++, --, **, =>, ::, ->, ..., ..=, ..

// Single-character
+, -, *, /, %, =, <, >, &, |, ^, !, ~, ?, :, ;, ., ,
```

### Example Tokenization

Input: `"if balances[msg.sender] >= amount { return true; }"`

Tokens:
1. `CodeKeyword("if")`
2. `CodeText(" ")`
3. `CodeIdentifier("balances", Some(topic))` - if "balances" is a known state variable
4. `CodeText("[")`
5. `CodeIdentifier("msg", None)` - built-in, not in topic_metadata
6. `CodeText(".")`
7. `CodeIdentifier("sender", None)` - property of msg
8. `CodeText("]")`
9. `CodeText(" ")`
10. `CodeOperator(">=")`
11. `CodeText(" ")`
12. `CodeIdentifier("amount", Some(topic))` - if "amount" is a known parameter
13. `CodeText(" { ")`
14. `CodeKeyword("return")`
15. `CodeText(" ")`
16. `CodeKeyword("true")`
17. `CodeText("; }")`

## Code Block Language Handling

For code blocks with a language specifier:

- `solidity`, `sol`: Full tokenization and reference resolution
- `javascript`, `js`, `typescript`, `ts`: Tokenize but don't resolve (different language)
- Other/none: Tokenize and attempt resolution (might be Solidity)

## Resolution Strategy

The existing `find_declaration_by_name` function can be reused for each identifier:

```rust
fn find_declaration_by_name(audit_data: &AuditData, value: &str) -> Option<&TopicMetadata>
```

Search order (already implemented):
1. Topic ID match
2. Qualified name match (e.g., "Contract.function")
3. Simple name match

## Formatter Updates

The formatter should use helper functions similar to `src/solidity/formatter.rs` for consistent styling.

### CSS Classes

| Node Type | CSS Class | Description |
|-----------|-----------|-------------|
| CodeKeyword | `keyword` | Language keywords (if, return, fn, etc.) |
| CodeOperator | `operator` | Operators (==, +=, &&, etc.) |
| CodeIdentifier (with ref) | `identifier reference` | Identifier that resolves to a declaration |
| CodeIdentifier (no ref) | `identifier` | Identifier without a known declaration |
| CodeText | (none or `text`) | Plain text, punctuation, whitespace |

### InlineCode Rendering

```html
<code class="inline-code">
  <span class="keyword">if</span>
  <span class="text"> (</span>
  <span class="identifier reference" data-ref-topic="N-456">participationState</span>
  <span class="text">[</span>
  <span class="identifier reference" data-ref-topic="N-789">addr</span>
  <span class="text">] </span>
  <span class="operator">==</span>
  <span class="text"> </span>
  <span class="identifier reference" data-ref-topic="N-101">ParticipationStatus</span>
  <span class="text">.</span>
  <span class="identifier reference" data-ref-topic="N-102">STARTED</span>
  <span class="text">) </span>
  <span class="keyword">return</span>
  <span class="text"> </span>
  <span class="keyword">true</span>
  <span class="text">;</span>
</code>
```

### CodeBlock Rendering

Similar structure within `<pre><code>` tags, preserving whitespace and newlines.

### Class Mapping (Parser - at parse time)

When `find_declaration_by_name` returns topic metadata, encode the CSS class directly into the `CodeIdentifier` node:

| TopicMetadata Kind | CSS Class |
|-------------------|-----------|
| `NamedTopicKind::Contract(_)` | `contract` |
| `NamedTopicKind::Function(_)` | `function` |
| `NamedTopicKind::Modifier` | `modifier` |
| `NamedTopicKind::Event` | `event` |
| `NamedTopicKind::Error` | `error` |
| `NamedTopicKind::Struct` | `struct` |
| `NamedTopicKind::Enum` | `enum` |
| `NamedTopicKind::EnumValue` | `enum-value` |
| `NamedTopicKind::StateVariable(Mutable)` | `mutable-state-variable` |
| `NamedTopicKind::StateVariable(Immutable)` | `immutable-state-variable` |
| `NamedTopicKind::StateVariable(Constant)` | `constant` |
| `NamedTopicKind::LocalVariable` | `local-variable` |
| `NamedTopicKind::Parameter` | `identifier` |
| `NamedMutableTopicKind::StateVariable` | `mutable-state-variable` |
| `NamedMutableTopicKind::LocalVariable` | `mutable-local-variable` |
| No match found | `identifier` |

**Parser logic for class determination:**

```rust
fn get_class_for_topic_metadata(metadata: &TopicMetadata) -> &'static str {
  match metadata {
    TopicMetadata::NamedTopic { kind, .. } => match kind {
      NamedTopicKind::Contract(_) => "contract",
      NamedTopicKind::Function(_) => "function",
      NamedTopicKind::Modifier => "modifier",
      NamedTopicKind::Event => "event",
      NamedTopicKind::Error => "error",
      NamedTopicKind::Struct => "struct",
      NamedTopicKind::Enum => "enum",
      NamedTopicKind::EnumValue => "enum-value",
      NamedTopicKind::StateVariable(VariableMutability::Mutable) => "mutable-state-variable",
      NamedTopicKind::StateVariable(VariableMutability::Immutable) => "immutable-state-variable",
      NamedTopicKind::StateVariable(VariableMutability::Constant) => "constant",
      NamedTopicKind::LocalVariable => "local-variable",
      NamedTopicKind::Parameter => "identifier",
      _ => "identifier",
    },
    TopicMetadata::NamedMutableTopic { kind, .. } => match kind {
      NamedMutableTopicKind::StateVariable => "mutable-state-variable",
      NamedMutableTopicKind::LocalVariable => "mutable-local-variable",
    },
    TopicMetadata::UnnamedTopic { .. } => "identifier",
  }
}
```

### Formatting Strategy for CodeIdentifier

Since the `class` is already encoded in the node, the formatter simply uses it:

```rust
fn format_code_identifier(
  node_id: i32,
  value: &str,
  referenced_topic: Option<&topic::Topic>,
  class: &str,
) -> String {
  match referenced_topic {
    Some(ref_topic) => {
      // Use format_topic_token for referenced identifiers
      let node_topic = topic::new_documentation_topic(node_id);
      formatting::format_topic_token(&node_topic, &formatting::html_escape(value), class, ref_topic)
    }
    None => {
      // No reference - just use format_token with the class
      formatting::format_token(&formatting::html_escape(value), class)
    }
  }
}
```

### Using Existing Formatting Functions

The formatter should use existing `formatting` module functions:

| Token Type | Function to Use |
|------------|-----------------|
| CodeKeyword | `formatting::format_token(value, "keyword")` |
| CodeOperator | `formatting::format_token(&formatting::html_escape(value), "operator")` |
| CodeIdentifier (no ref) | `formatting::format_token(&formatting::html_escape(value), &node.class)` |
| CodeIdentifier (with ref) | `formatting::format_topic_token(node_topic, value, &node.class, ref_topic)` |
| CodeText | `formatting::html_escape(value)` (no span wrapper needed) |

## Files to Modify

1. **`src/documentation/parser.rs`**
   - Add `CodeKeyword`, `CodeOperator`, `CodeIdentifier`, and `CodeText` enum variants to `DocumentationNode`
   - Modify `InlineCode` to have `children` instead of `referenced_declaration`
   - Modify `CodeBlock` to have `children`
   - Add `tokenize_code()` function with keyword/operator/identifier detection
   - Add `SOLIDITY_KEYWORDS`, `RUST_KEYWORDS`, and `OPERATORS` constants
   - Update `convert_mdast_node()` for InlineCode and Code handling
   - Update `children_to_stubs()` for new node types
   - Update `DocumentationNode::children()` for InlineCode and CodeBlock
   - Update `DocumentationNode::node_id()` for new node types

2. **`src/documentation/formatter.rs`**
   - Update InlineCode rendering to iterate children using `formatting` module functions
   - Update CodeBlock rendering to iterate children using `formatting` module functions
   - Add rendering cases for CodeKeyword, CodeOperator, CodeIdentifier, and CodeText
   - For CodeIdentifier with reference: use `formatting::format_topic_token` with class based on topic metadata kind
   - For CodeIdentifier without reference: use `formatting::format_token` with "identifier" class
   - For CodeKeyword: use `formatting::format_token` with "keyword" class
   - For CodeOperator: use `formatting::format_token` with "operator" class
   - For CodeText: use `formatting::html_escape` (no wrapper span)
   - Pass `topic_metadata` to formatter for class lookup

3. **`src/documentation/analyzer.rs`**
   - Handle new node types (no topic_metadata needed, just add to nodes map)

## Edge Cases

1. **Empty inline code**: `` ` ` `` → Empty children array
2. **Only punctuation**: `` `[]` `` → Single CodeText child
3. **Qualified names**: `` `Contract.function` `` → Three nodes: CodeIdentifier, CodeText("."), CodeIdentifier
4. **Numbers**: `123` → CodeText (not an identifier)
5. **String literals**: `"hello"` → CodeText (including quotes)
6. **Keywords**: `if`, `else`, `return`, `fn`, `pub` → CodeKeyword
7. **Operators**: `==`, `+=`, `&&` → CodeOperator
8. **Mixed operators and punctuation**: `a[i] += 1` → identifier, text, identifier, text, operator, text, text
9. **Rust-style paths**: `std::io::Read` → identifier, operator(::), identifier, operator(::), identifier
10. **Comments in code blocks**: `// comment` or `/* comment */` → Could be CodeText or special handling

## Performance Considerations

1. Tokenization is O(n) where n is code length
2. Resolution is O(m) where m is number of identifiers × topic_metadata size
3. Consider caching resolved identifiers if the same name appears multiple times
4. For large code blocks, consider lazy resolution

## Future Enhancements

1. **Scope-aware resolution**: Consider the documentation's context when resolving ambiguous names
2. **Member access chains**: Resolve `contract.variable.field` as a chain
3. **Function signatures**: Parse and resolve function calls with parameters
4. **Type annotations**: Resolve type names in declarations
