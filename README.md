# fortfront-new

Production generated frontend for the Lazy Fortran compiler program.

This repository will own the generated lexer, parser, AST, source-linked
diagnostics, and frontend-to-MIR boundary. The laboratory repository at
`../lazy-fortran-new` owns the roadmap, decisions, experiments, runs,
provenance manifests, and cross-repository wiring.

The repository starts with the `frontend-v0` contract scaffold. It must remain
production-only: do not add research ledgers, experiment reports, model logs,
or copied external source material here.

## Build

```sh
fo
tools/regenerate_generated_ast.sh
python3 tools/test_generated_ast.py
tools/regenerate_generated_ast_v1.sh
python3 tools/test_generated_ast_v1.py
python3 tools/test_generated_assignment_policy.py
```

The regeneration command reads `../lazy-fortran-new/contracts/frontend-ast-v0.sxs`
and writes `src/generated/frontend_ast_v0_generated.f90`. The schema is not
copied into this repository.

The v1 command reads `../lazy-fortran-new/contracts/frontend-ast-v1.sxs` and
writes `src/generated/frontend_ast_v1_generated.f90`. The narrow raw-source
producer is reproducible with `fo exec fortfront-source-ast-v1 <source> <output>`.
It emits canonical v1 SX only for the promoted `program p` declaration witness.

The generic grammar runtime consumes an external line-oriented
standardir-grammar-v0 SX file. Pass the start LHS and zero or more token names
after the file path:

    fo exec fortfront-grammar-runtime <grammar-file> <start-lhs> [token ...]

It reports the parsed rule and line counts and the final accepted, rejected,
ambiguous, unresolved, or malformed outcome.
