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
```

The regeneration command reads `../lazy-fortran-new/contracts/frontend-ast-v0.sxs`
and writes `src/generated/frontend_ast_v0_generated.f90`. The schema is not
copied into this repository.
