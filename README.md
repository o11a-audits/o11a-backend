# o11a-backend
The o11a backend has five modules that form an almost linear processing pipeline:

1. Parser - parses the audit source and allows clients to post new text blobs (added comments and docs) to be parsed in the context of the audit
2. Analyzer - analyzes the audit source, producing a directory of definitions and their attributes, allowing clients to fetch structured audit data
3. Formatter - transforms the AST into formatter nodes
4. Collaborator - stores discussion topics and the comments under them, allowing clients to post new comments and approve or disapprove of comments from other users
5. Checker - checks variable constraints and allows clients to post new constraints to be checked, providing data on any conflicting constraints

# Parser
Audit source files are parsed and compiled by the Foundry compilers, which can output an AST in JSON format that the processor can work with. Most analysis is done with this AST, with few references to the source files. Currently, there are no plans to support audit source files not supported by the Foundry tooling. Markdown files are parsed into an AST similar to source files, with paragraphs becoming declarations and code snippets becoming references. Since most of this work is accomplished through external libraries, it is the simplest part of the o11a processor.

# Analyzer
The analyzer is responsible for traversing the Abstract Syntax Tree and creating a directory of in-scope declarations and declarations used by in-scope members for the audit. Each declaration has many attributes, which are the result of static analysis. The core attributes are the topic ID, name, scope, and declaration kind. Depending on the kind of declaration, other attributes exist that are stored in different directories by topic ID:

- Signature directories: store the signatures of source, text, and comment declarations. This is the value to display to the user that represents the declaration. For a function, this is the code for the function without the body. For a text declaration, this is the text itself. Signatures are rendered in the same way as source code, so any info comments or references in them will be rendered.
- Calls directories: store the external calls in a function (or contract by association)

Declarations are scoped by three properties: Container, Component, and Member. Using the scope, any identifier/operation or its parent can be linked to.

For contract source files, the container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member. For documentation, the container is the source file, the component is a section, and the member is a paragraph. For comments, the container is the comment ID, the component is a section, and the member is a paragraph.

Projects being audited can pull in lots of dependencies, yet only use a few functions from them. Naively processing all contracts/functions from every file in the project leads to excessive processing and bloating in the audit. To avoid this, we can take the following two-pass approach to make sure only in-scope and used by in-scope contracts/functions are processed:
1. First read and parse each AST (one at a time, if low on memory), storing its declarations in an accumulating simple declaration dictionary that stores all declarations in the audit, with a list of the other nodes each references in its body and a list of errors and revert statements it calls, making note of whether the node is from an in-scope ast or not. This is the first pass, and is a great place to do processing that needs to check all child nodes recursively.
2. Loop over all the publicly (public, external, or internal; NOT private) in-scope declarations gathered, storing them in a new dictionary that stores all publicly in-scope declarations in the audit and the nodes that reference them. When a declaration is found to be publicly in-scope, add it to the in-scope declaration dictionary with the node that referenced it and look up its references in the previously generated dictionary. Add each of its references to the accumulating dictionary, then recursively check them for their references, adding them as needed and so on.
3. Now with a dictionary of all in-scope and used by in-scope declarations, we can parse each AST in scope into memory one at a time, checking each declaration for inclusion in the in-scope dictionary. If it is, we add it to an accumulating dictionary of detailed declarations. This is the second pass, and it's a great place to perform processing that requires knowledge of a node's references.

# Formatter
The formatter takes an AST node and its source file as inputs and returns HTML. This HTML is largely formatted without consideration of the source file; however, the output needs to retain the extra whitespace of the source file. Extra whitespace is often used to group statements and has some semantic meaning that is not reflected in the AST.

This rendered HTML is designed to be forty characters wide. Forty characters allows for four columns of code to be displayed side-by-side on a typical screen while not being so small that the comments are difficult to read. (Forty characters also allow for two columns of code to be printed on standard paper.)

There are two core primitives for the formatter to respect: identifiers and operators. Each identifier and each operator will have dedicated topics for discussion, so they each need to be set on different lines so that their comments can be displayed inline above them. Identifier and operator inline comments are formatted differently, so there can be both an identifier and an operator on the same line, but the same line cannot have two identifiers or two operators.

There is only one way to format each expression because there are strict per-line formatting rules, making the formatter output very vertical but straightforward to implement.

When any declaration/reference, or operator line is rendered to HTML, it will have an empty span element before it, allowing the client to inject info comments into that element dynamically. Because the code width is set to 40 characters, formatting of the inline comments to be injected into the HTML is straightforward as well.

The formatter output does not include traditional line numbers because the formatter is aggressive in changing the source text, and the API is designed to enable clients to display many smaller snippets of code. Clients are not expected to show complete source files in regular use, so the original line numbers are not particularly meaningful. Because of this, if a gutter is shown, it will have operation numbers instead of line numbers.

Although the complete source code of a file is not used, clients may want to display or allow copying the full source file in a separate view, at the user's request, for niche purposes. It should not be interactive.

# Collaborator

# Checker

Functions need to track named return values and side effects for checking.

Constraint boundaries are places where two or more variables interact with each other, and are the places where constraints are checked and enforced. Constraint boundaries are:
 - Function/struct arguments (checked from the argument variable to the parameter variable)
 - Variable assignment (checked from the value expression to the variable)
 - Binary operators (checked on the operands, that they have compatible units and semantics)
 - If else/ternary operators (checked that the condition matches the semantics of both branches)

### Semantic and Logical Constraint Checks
Patterns annotate logical constraint checks and can be checked by a constraint algorithm. Semantic constraint checks are annotated in regular language with business logic and can only be checked by something that understands the specific business logic semantics.

General audit flow is:
1. Read and understand the docs and the purpose of the project
2. Read the code, noting what every variable is, is used for, and its constraints (values that cause error states), cross-referencing the docs
3. Step through all constraint boundaries, checking that the constraints hold up correctly by logic and semantics
