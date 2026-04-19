# Miva Developer Guide

## Build

```bash
dune clean    # Clean artifacts
dune build    # Build compiler
```

Requires OCaml 4.14, dune, and menhir. Use `eval $(opam env)` if using opam.

## Project Structure

- `lib/` - Compiler core (lexer, parser, semantic analysis, codegen)
- `bin/` - CLI tools (main entry point)
- `test-mvp/` - Example Miva project to test the compiler
- `build/` - Build output
- `_build/` - Dune build artifacts

## Entry Point

`bin/main.ml` is the compiler CLI. The library entry is `lib/` with modules: `lexer.mll`, `parser.mly`, `semantic.ml`, `codegen.ml`, `typegen.ml`.

## Notes

- Uses Menhir for parser generation (`lib/parser.mly`) and OCamllex for lexer (`lib/lexer.mll`)
- Parser conflicts are in `lib/parser.conflicts`
- Run `dune build` from repo root; dune config is in `dune-project`
