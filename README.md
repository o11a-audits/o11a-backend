# o11a-processor
The o11a processor is responsible for four things in a linear processing pipeline:

1. Parsing the audit source
2. Analyzing the audit source
3. Transforming the AST into a Formatter Node Tree
4. Providing an API for clients to fetch structured audit data from, post and check constraints with, and post new text blobs to be parsed.

# Parsing the audit source
Audit source files are parsed and compiled by the Foundry compilers, which return an AST for the preprocessor to work with. Most analysis is done with this AST, with few references to the source files. Currently, there are no plans to support audit source files not supported by the Foundry tooling. Markdown files are parsed into an AST similar to source files, with paragraphs becoming declarations and code snippets becoming references.

Items in the audit source files are scoped into three levels: Container, Component, and Member. Using the scope, any declaration or its parent can be linked to.

For contract source files: The container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member.

For markdown files: The container is the source file, the component is a section, and the member is a paragraph.

For comments: The container is the comment ID, the component is a section, and the member is a paragraph.

# Analyzing the audit source

# Formatting the audit source

# External API
