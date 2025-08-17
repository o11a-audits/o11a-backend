# o11a-backend
The o11a backend has five modules that form an almost linear processing pipeline:

1. Parser - parses the audit source and allows clients to post new text blobs (added comments and docs) to be parsed in the context of the audit.
2. Analyzer - analyzes the audit source, producing a directory of definitions and their attributes, allowing clients to fetch structured audit data.
3. Formatter - transforms the AST into formatter nodes.
4. Collaborator - stores discussion topics and the comments under them, allowing clients to post new comments and approve or disapprove of comments from other users.
5. Checker - checks variable constraints and allows clients to post new constraints to be checked, providing data on any conflicting constraints.

# Parser
Audit source files are parsed and compiled by the Foundry compilers, which return an AST for the processor to work with. Most analysis is done with this AST, with few references to the source files. Currently, there are no plans to support audit source files not supported by the Foundry tooling. Markdown files are parsed into an AST similar to source files, with paragraphs becoming declarations and code snippets becoming references. Because most of this work is done via external libraries, it is the simplest part of the o11a processor.

# Analyzer
The analyzer is the most complex module in the processor. It is responsible for traversing the Abstract Syntax Tree and creating a directory of in-scope declarations and declarations used by in-scope members for the audit. Each declaration has many attributes, which are the result of static analysis. The core attributes are the topic ID, name, scope, and declaration kind. Depending on the kind of declaration, other attributes exist that are stored in different directories by topic ID:

- Signature directories: store the signatures of source, text, and comment declarations. This is the value to display to the user that represents the declaration. For a function, this is the code for the function without the body. For a text declaration, this is the text itself. Signatures are rendered in the same way as source code, so any info comments or references in them will be rendered.

Declarations are scoped by three properties: Container, Component, and Member. Using the scope, any declaration or its parent can be linked to.

For contract source files, the container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member. For documentation, the container is the source file, the component is a section, and the member is a paragraph. For comments, the container is the comment ID, the component is a section, and the member is a paragraph.


# Formatter

# Collaborator

# Checker
