# Graph Library — Declarative Graph Rendering ⚙️📡

Technical Haskell library for declarative graph data structures, deterministic
layout algorithms, and a composable rendering pipeline with a minimal GUI
front-end for visualization and experimentation. Focus is on clear separation
between pure model, rendering logic, and a thin UI layer. 🧠🔬🖥️

## Purpose

This repository implements a small, focused graph library intended for
experimentation and research: core graph types and algorithms are implemented
purely, rendering and layout live in a dedicated module, and the GUI is a
lightweight integration layer suitable for manual testing and demos. ⚙️

## Repository structure

- `graph-library.cabal` — package manifest and build metadata.
- `Graph.hs` — core graph model: types, pure algorithms, transformations, and
  traversal utilities. Keep algorithmic logic here for testability. 🧠
- `Render.hs` — layout and rendering primitives: coordinate transforms,
  layout helpers, and rendering pipeline (SVG/export helpers). 🔬
- `GUI.hs` — minimal application entry point and integration with the
  renderer; thin layer for I/O and interactive inspection. 🖥️
- `dist-newstyle/` — cabal build artifacts and caches (auto-generated).
  Inspect when you need exact compiler/build outputs. 📦

## Build & Run

Recommended: use `cabal v2-build` and `cabal v2-run` (GHC-compatible).

```bash
cabal v2-update
cabal v2-build
cabal v2-run gui
```

For iterative development use `ghcid` or `cabal repl` for fast feedback. 🧪

## Design notes

- Separation of concerns: keep pure functions and algorithms in `Graph.hs`.
- Rendering is deterministic and testable; add unit/integration tests around
  layout outputs where possible. 🛠️
- The GUI should remain a thin orchestration layer — avoid embedding core
  logic there.

## Developer tooling

- Use `hlint` for suggestions and `ormolu`/`stylish-haskell` for formatting.
- Add unit tests for algorithmic behavior (property-based tests recommended).

## Contributing

- Open an issue describing the proposal or bug.
- Add focused tests for any algorithmic changes.
- Keep changes minimal and document design choices in PR descriptions.

## License

See `graph-library.cabal` for license metadata. 📜

---

If you want, I can: add a CONTRIBUTING file, create a small test harness,
or commit this `README.md` directly — tell me which you'd prefer. ✅
