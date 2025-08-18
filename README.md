# o11a-backend
The o11a backend has five modules that form an almost linear processing pipeline:

1. Parser - parses the audit source and allows clients to post new text blobs (added comments and docs) to be parsed in the context of the audit
2. Analyzer - analyzes the audit source, producing a directory of definitions and their attributes, allowing clients to fetch structured audit data
3. Formatter - transforms the AST into formatter nodes
4. Collaborator - stores discussion topics and the comments under them, allowing clients to post new comments and approve or disapprove of comments from other users
5. Checker - checks variable constraints and allows clients to post new constraints to be checked, providing data on any conflicting constraints

# Parser
Audit source files are parsed and compiled by the Foundry compilers, which return an AST for the processor to work with. Most analysis is done with this AST, with few references to the source files. Currently, there are no plans to support audit source files not supported by the Foundry tooling. Markdown files are parsed into an AST similar to source files, with paragraphs becoming declarations and code snippets becoming references. Because most of this work is done via external libraries, it is the simplest part of the o11a processor.

# Analyzer
The analyzer is the most complex module in the processor. It is responsible for traversing the Abstract Syntax Tree and creating a directory of in-scope declarations and declarations used by in-scope members for the audit. Each declaration has many attributes, which are the result of static analysis. The core attributes are the topic ID, name, scope, and declaration kind. Depending on the kind of declaration, other attributes exist that are stored in different directories by topic ID:

- Signature directories: store the signatures of source, text, and comment declarations. This is the value to display to the user that represents the declaration. For a function, this is the code for the function without the body. For a text declaration, this is the text itself. Signatures are rendered in the same way as source code, so any info comments or references in them will be rendered.
- Calls directories: store the external calls in a function (or contract by association)

Declarations are scoped by three properties: Container, Component, and Member. Using the scope, any declaration or its parent can be linked to.

For contract source files, the container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member. For documentation, the container is the source file, the component is a section, and the member is a paragraph. For comments, the container is the comment ID, the component is a section, and the member is a paragraph.


# Formatter
The formatter takes an AST node and the source file as the inputs and returns a tree of formatter nodes as HTML. This HTML is sent to the browser so it can dynamically render the source text based on the width of the container it is present in. 

Should the formatter have a naive "parents always split first" approach, or a more sophisticated approach where all children are traversed first, then all ranked by node type so a child could potentially split before its parent?

There can always only be one declaration per line to give space for inline comments above it.

The formatter nodes can be:

- A text node with a class for syntax highlighting (keywords, operators, literals)
- A reference node (variable reference, function calls)
- A declaration node (variable declaration, function definition, contract definition)
- A split node (can be rendered as a space or a newline depending on container width)
- A block node (contains other nodes with an indent)

All nodes have both a full and a condensed value, except for references, declaration names, and some split nodes that are always a space.

Formatting starts aggressively and progresses to a condensed version of the code. For example, here is a comparison between vanilla solidity, the formatter's full output, and the condensed output:
``` solidity
// Vanilla solidity
bytes32 public immutable myBytes;
// o11a formatter output
pub immut myBytes;
// o11a condensed output
pi myBytes
```
``` solidity
// Vanilla solidity
bytes32 public constant CAMPAIGN_ADMIN_ROLE = keccak256("CAMPAIGN_ADMIN_ROLE");
// o11a formatter output
pub const CAMPAIGN_ADMIN_ROLE = keccak256("CAMPAIGN_ADMIN_ROLE");
// o11a condensed output
pc CAMPAIGN_ADMIN_ROLE
```
``` solidity
// Vanilla solidity
if (startTimestamp_ != 0 && startTimestamp_ <= block.timestamp) {
    revert InvalidCampaignSettings(block.timestamp);
}

// o11a formatter output
if (
  startTimestamp_ != 0
  && startTimestamp_ <= block.timestamp
) {
  rev InvalidCampaignSettings(block.timestamp);
}

// o11a condensed output
if (
 startTimestamp_
  != 0
 && startTimestamp_
  <= block.timestamp
) {
 rev
}
```

The formatter output is more concise than Solidity, but it is complete. The condensed view is not complete, but can be displayed in tiny containers. The semicolon is missing from all condensed lines to designate that it is not a complete line of code. Hovering over the line will show the complete line.

# Collaborator

# Checker
