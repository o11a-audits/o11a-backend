# o11a-backend
The o11a backend has five modules that form an almost linear processing pipeline:

1. Parser - parses the audit source and allows clients to post new text blobs (added comments and docs) to be parsed in the context of the audit
2. Analyzer - analyzes the audit source, producing a directory of definitions and their attributes, allowing clients to fetch structured audit data
3. Formatter - transforms the AST into formatter nodes
4. Collaborator - stores discussion topics and the comments under them, allowing clients to post new comments and approve or disapprove of comments from other users
5. Checker - checks variable constraints and allows clients to post new constraints to be checked, providing data on any conflicting constraints

# Parser
Audit source files are parsed and compiled by the Foundry compilers, which can output a AST in JSON format that the processor can work with. Most analysis is done with this AST, with few references to the source files. Currently, there are no plans to support audit source files not supported by the Foundry tooling. Markdown files are parsed into an AST similar to source files, with paragraphs becoming declarations and code snippets becoming references. Because most of this work is done via external libraries, it is the simplest part of the o11a processor.

# Analyzer
The analyzer is the most complex module in the processor. It is responsible for traversing the Abstract Syntax Tree and creating a directory of in-scope declarations and declarations used by in-scope members for the audit. Each declaration has many attributes, which are the result of static analysis. The core attributes are the topic ID, name, scope, and declaration kind. Depending on the kind of declaration, other attributes exist that are stored in different directories by topic ID:

- Signature directories: store the signatures of source, text, and comment declarations. This is the value to display to the user that represents the declaration. For a function, this is the code for the function without the body. For a text declaration, this is the text itself. Signatures are rendered in the same way as source code, so any info comments or references in them will be rendered.
- Calls directories: store the external calls in a function (or contract by association)

Declarations are scoped by three properties: Container, Component, and Member. Using the scope, any declaration or its parent can be linked to.

For contract source files, the container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member. For documentation, the container is the source file, the component is a section, and the member is a paragraph. For comments, the container is the comment ID, the component is a section, and the member is a paragraph.

Projects being audited can pull in lots of dependencies, yet only use a few functions from them. Naively processing all contracts/functions from every file in the project leads to a lot of unnecessary processing and bloat in the audit. To avoid this, we can take the following two-pass approach to make sure only in-scope and used by in-scope contracts/functions are processed:
1. First read and parse each AST (one at a time, if low on memory), storing its declarations in an accumulating simple declaration dictionary that stores all declarations in the audit, with a list of the other nodes each references in its body and a list of errors and revert statements it calls, making note of whether the node is from an in-scope ast or not. This is the first pass, and is a great place to do processing that needs to recursively check all child nodes.
2. Loop over all the publicly (public, external, or internal; NOT private) in-scope declarations gathered, storing them in a new dictionary that stores all publicly in-scope declarations in the audit and the nodes that reference it. When a declaration is found to be publicly in-scope, add it to the in-scope declaration dictionary with the node that referenced it and look up its references in the previously generated dictionary. Add each one of its references to the accumulating dictionary, then check them recursively for their references, adding them and so on.
3. Now with a dictionary of all in-scope and used by in-scope declarations, we can parse each AST in scope into memory one at a time, checking each declaration for inclusion in the in-scope dictionary. If it is, we add it to an accumulating dictionary of detailed declarations. This is the second pass, and is a great place to do processing that needs knowledge of a node's references.

# Formatter
The formatter takes an AST node and its source file as inputs and returns a tree of formatter nodes as HTML. These AST nodes are largely formatted without consideration of the source file; however, the output does need to retain the comments and extra whitespace of the original source file. Extra whitespace is often used to group statements and has some semantic meaning, which is missing from the AST. This HTML is sent to the browser so it can dynamically render the source text based on the width of the container it is present in.

Should the formatter have a naive "parents always split first" approach, or a more sophisticated approach where all children are traversed first, then all ranked by node type so a child could potentially split before its parent?

There can always only be one declaration per line to give space for inline comments above it. The same for function arguments, so the info comments of the descendants can be shown inline above the ancestors. When declaration or function argument lines are rendered to HTML, they have an empty span element before them so the client can dynamically inject any info comments above them. It has to be a sibling to the source line so that it can use the native word-wrapping while the source line uses the custom formatter.

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
``` solidity
// Vanilla solidity
_transfer(rewardToken, participation.userAddress, userRewards);

// o11a formatter output
_transfer(
  rewardToken,
  participation.userAddress,
  userRewards
);

// o11a condensed output
_transfer(
 rewardToken,
 participation
  .userAddress,
 userRewards
);
```

The formatter output is more concise than Solidity, but it is complete. The condensed view is not complete, but can be displayed in tiny containers. The semicolon is missing from all condensed lines to designate that it is not a complete line of code. Hovering over the line will show the complete line.

Because the formatter output is aggresive at changing the original source text and the API is designed to enable clients to show many smaller snippets of code, the formatter output does not include line numbers. Clients are not expected to show full source files in regular use. They may want to show or allow copying the full source file under a separate view by request of the user for niche uses, but it should not be interactive.

# Collaborator

# Checker

Constraint boundaries are places where two or more variables interact with each other, and are the places where constraints are checked and enforced. Constraint boundaries are:
 - Function / struct arguments (checked from the argument variable to the parameter variable)
 - Variable assignment (checked from the value expression to the variable)
 - Binary operators (checked on the operands, that they have compatible units and semantics)
 - If else / ternary operators (checked that the condition matches the semantics of both branches)

### Semantic and Logical Constraint Checks
Logical constraint checks are annotated by patterns and can be checked by a constraint algorithm. Semantic constraint checks are annotated in regular language with business logic and can only be checked by something that understands the specific business logic semantics.

General audit flow is:
1. Read and understand the docs and purpose of the project
2. Read the code, noting what every variable is, is used for, and its constraints (values that cause error states), cross referencing the docs
3. Step through all constraint boundaries, checking that the constraints hold up correctly by logic and semantics
