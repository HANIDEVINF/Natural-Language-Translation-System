# Lexical, Syntax and Semantic Analyzer — Flex/Bison Project

Despite the repository name, the source files describe a small compiler front-end for a Pascal-like language, not a natural-language machine-translation system.

## Included files

- `lex.l` — Flex lexical rules for keywords, identifiers, constants, strings, operators and comments.
- `syn.y` — Bison grammar with syntax and semantic checks for declarations and statements.
- `syntaxique (2).y` — another grammar/version snapshot; compare it with `syn.y` before deciding which one is canonical.
- `ts (7).h` — symbol-table-related header snapshot.
- `TEST22.txt` — sample input program.

The grammar includes constructs such as PROGRAM, VAR, CONST, BEGIN/END, INTEGER/REAL/STRING, conditionals, loops and read/write statements.

## Current build status

This repository is a coursework source snapshot, not yet a reproducible build package. The lexer includes `syn.tab.h` and `ts.h`, and also references `ts_quad.h`; the root currently contains `ts (7).h` rather than `ts.h`, and the referenced `ts_quad.h` is not present in the root listing. Therefore, a successful build cannot be promised from the current tree without resolving these dependencies and choosing the canonical grammar.

## Suggested build workflow after dependencies are repaired

1. Choose the canonical grammar and make header/source names consistent.
2. Generate the parser header and source with Bison.
3. Generate the scanner with Flex after the parser header exists.
4. Compile the generated C files together with the symbol-table and quadruple implementation.
5. Run the compiler against `TEST22.txt` and add cases for valid programs, undeclared identifiers, duplicate declarations, incompatible types and malformed syntax.

This README documents the source files currently present; it does not claim that the build or semantic checks have been independently tested.
