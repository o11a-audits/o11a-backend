# o11a-preprocessor
The o11a source code preprocessor is responsible for four things in a linear processing pipeline:

1. Parsing the audit source
2. Analyzing the audit source
3. Formatting the audit source
4. Providing an API for clients to fetch structured audit data from

# Parsing the audit source
Audit source files are parsed and compiled by the Foundry compilers, which returns an AST for the preprocessor to work with. Most analysis is done with this AST, with few references to the source files.

# Analyzing the audit source

# Formatting the audit source

# External API
