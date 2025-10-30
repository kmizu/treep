# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**treep** is a mini programming language implemented in Scala 3 with a clear educational pipeline: surface syntax → EAST normalization → macro expansion → Hindley-Milner type inference → interpreter execution. The language uses `.treep` file extension and focuses on demonstrating hygienic macros, row polymorphic records, and extension methods.

## Development Commands

### Build & Compile
```bash
sbt compile
```

### Testing
```bash
sbt test                    # Run all MUnit tests
```

### CLI Commands
```bash
sbt "run new"               # Generate samples/hello.treep
sbt "run build"             # Parse, expand macros, typecheck all .treep files
sbt "run run"               # Execute all .treep files with zero-arg main()
sbt "run run path/to/file.treep"  # Execute specific file
sbt "run fmt"               # Formatter (currently no-op)
sbt "run test"              # Test integration (not wired yet)
```

### Workflow
Always run `sbt compile` → `sbt test` after making changes to ensure build and tests pass.

## Architecture

### Pipeline Flow
The processing pipeline is strictly ordered:
1. **Lexer** (`lexer/Lexer.scala`) → tokens
2. **Parser** (`parser/Parser.scala`) → CST (Concrete Syntax Tree)
3. **Normalize** (`east/Normalize.scala`) → EAST (Element AST)
4. **Macro** (`macro/Macro.scala`) → expanded EAST with hygienic gensym
5. **Type Checker** (`types/Checker.scala` + `types/HM.scala`) → Algorithm W type inference
6. **Interpreter** (`interpreter/Interpreter.scala`) → execution

### Key Data Structures

**CST (parser/CST.scala)**: Surface syntax representation with `Program`, `TopDecl` (FunDef, ConstDecl, ModuleDecl, StructDef), `Stmt`, and `Expr` nodes. Includes span tracking for error reporting.

**EAST (east/EAST.scala)**: Normalized element-based AST. All nodes are `Element(kind, name, attrs, children, span)`. This uniform structure simplifies macro expansion and type checking. The `kind` field identifies node types ("def", "let", "call", "int", etc.).

**Type System (types/Types.scala, types/HM.scala)**:
- Algorithm W (Hindley-Milner) with support for:
  - Base types: `Int`, `Bool`, `String`, `Unit`
  - Collections: `List[A]`, `Dict[K,V]`, `Iter[T]`
  - Functions: `A -> B` (right-associative)
  - Tuples: `(A, B)`
  - Row polymorphic records: `{ x: T | ρ }` for field access with incomplete records
- Builtin environment in `HM.builtinEnv` defines polymorphic types for operators and standard library functions
- Extension methods resolved as: builtin → record function field → top-level function with receiver as first arg

**Macro Expansion (macro/Macro.scala)**:
- Hygienic macro system using gensym (`__it$N`, `__tmp$N`) to avoid variable capture
- Key expansion: `for (x in: xs)` desugars to `while` loop with iterator protocol (`iter()`, `hasNext()`, `next()`)
- Operates on EAST, returning expanded EAST

### CLI Entry Point (cli/Main.scala)

The `Main` object handles all CLI commands:
- **File discovery**: Searches from `DefaultRoots` (samples/, current dir), excludes `.git`, `.idea`, `.bloop`, `.metals`, `target`, `out`
- **Analysis pipeline**: `analyzeFile()` runs full pipeline and collects parse/type diagnostics
- **Zero-arity main detection**: `hasZeroArityMain()` checks if program has `def main() returns: T` with no params
- Uses relative path display for cleaner error messages

### Standard Library

Implemented as builtin functions in `HM.builtinEnv` and interpreter runtime:
- **Collections**: `List[A]` with `length()`, `head()`, `tail()`, `push(a)`, `append(a)`, `concat(xs)`, `iter()`
- **Dictionaries**: `Dict[K,V]` with `size()`, `keys()`, `values()`, `entries()`, `hasKey(k)`, `get(k)`, `getOrElse(k,d)`, `put(k,v)`, `remove(k)`, `iter()`
- **Iterators**: `Iter[T]` with `hasNext()`, `next()`, `toList()` (used by `for..in` syntax)
- **Tuples**: `fst((A,B))`, `snd((A,B))`
- **IO**: `print(value)`, `println(value)` return `Unit`

### Language Features

**Function Syntax**:
- Definition: `def name(x: Int) returns: Int { ... }`
- Lambda: `(x: Int) -> { ... }`
- Function type: `Int -> Int` (right-associative)

**Type Inference**:
- Automatic inference for let bindings: `let x = 42` infers `Int`
- Explicit annotations optional: `let x: Int = 42`
- Polymorphic functions automatically generalized

**Method Resolution Order**:
1. Builtin methods (length, push, etc.)
2. Record function fields (`record.field` where field is a function)
3. Top-level functions with receiver as first argument

**Row Polymorphism**:
Records support incomplete field specifications: `{ x: T | ρ }` allows accessing `x` without knowing all fields. Enables flexible record extension and dynamic dictionary access.

## Testing

Tests use MUnit framework. Test files should mirror source structure. When adding features:
- Add parser tests for new syntax
- Add type checker tests for type rules
- Add interpreter tests for runtime behavior
- Add sample `.treep` files in `samples/` for integration testing

## Code Style & Communication

From AGENTS.md:
- **Response tone**: Always respond in "はんなり北摂弁" (gentle Hokusetsu dialect)
- **Persona**: "優しい性格の幼馴染" (kind childhood friend) - warm, considerate, encouraging but maintaining courtesy
- **Integrity**: Work with honesty and care. Never skip verification steps or make assumptions without checking.
- **Testing discipline**: Run `sbt test` after every change to verify nothing is broken. This is mandatory, not optional.
- Follow Conventional Commits: `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`, `test:`
- Keep commits focused and PRs small

## Common Patterns

**Adding a new builtin function**:
1. Add type scheme in `HM.builtinEnv` (use `poly()` for generics)
2. Add runtime implementation in `Interpreter.builtins`
3. Add tests in appropriate test file

**Adding new syntax**:
1. Extend CST in `parser/CST.scala`
2. Update parser in `parser/Parser.scala`
3. Add normalization rule in `east/Normalize.scala`
4. Consider if macro expansion needed in `macro/Macro.scala`
5. Add type checking rule in `types/Checker.scala`
6. Add interpreter case in `interpreter/Interpreter.scala`

**Error Reporting**:
- Parser errors collected in `Parser.lastErrors` as `ParseDiag(message, line, col)`
- Type errors returned from `Checker.check()` as `List[Diag(msg, path)]`
- Use path breadcrumbs for nested expressions: `path :+ "list-elem"`
