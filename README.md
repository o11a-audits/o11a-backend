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

Declarations are scoped by three properties: Container, Component, and Member. Using the scope, any declaration or its parent can be linked to.

For contract source files, the container is the source file, the component is a contract, and the member is a function. A contract's scope will only be a container, a function's scope will be a container and a component, and a local variable's scope will be a container, component, and member. For documentation, the container is the source file, the component is a section, and the member is a paragraph. For comments, the container is the comment ID, the component is a section, and the member is a paragraph.


# Formatter
The formatter takes an AST node as the input and returns a tree of formatter nodes as HTML. These nodes are sent to the browser as HTML elements so the browser can dynamically render the nodes based on the width of the container they are present in. 

Should the formatter have a naive "parents always split first" approach, or a more sophisticated approach where all children are traversed first, then all ranked by node type so a child could potentially split before its parent?

The formatter HTML elements are either text with a class representing any highlighting, or a whitespace node with a class representing its split rank. The lower split rank whitespace nodes will be changed from rendering as a space to a newline before the higher split ranks.

There can always only be one declaration per line to give space for inline comments above it.

The dynamic formatting works as follows:
The source text is rendered in a container. A JavaScript function initializes an internal state with a counter of 0 and an empty list of prior splits. It then runs a check on scrollWidth vs clientWidth to determine if the source text is overflowing horizontally. This check is attached to a resize event and will continually shrink or grow the source text as the container shrinks or grows. The check is as follows:
If the text is overflowing:
1. Search for the .split<count> elements and their text to a newline to split the line. If none exist, do nothing further
2. Increment the counter by 1, indicating that it has split the element at that rank
3. Save the width that triggered this overflow, along with the split rank that the width triggered
4. Run again until the text is not overflowing
If the text is not overflowing:
1. Check if the width is greater than the most recent (smallest) width that caused an overflow. If it is not, do nothing further
3. Search for the .split<count> elements, changing their text to a space
4. Decrement the counter by 1.
5. Run again until the width is not greater than the most recent width that caused an overflow

# Collaborator

# Checker
