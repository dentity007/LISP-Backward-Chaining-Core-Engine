# Car Troubleshooting Expert System (Backward Chaining)

This directory contains the modular backward-chaining expert system:

- `expert-system.lisp` — Core goal-driven inference engine with MYCIN-style certainty factors
- `car-rules.lisp` — Automotive diagnostic rules and questions
- `run.lisp` — Interactive consultation UI and demos
- `demo.lisp`, `test.lisp` — Demos and quick tests

For the full introduction, architecture, and walkthrough, see the project root README: `../README.md`.

## Quick Start

```lisp
;; From project root or this directory
(load "car-expert-system/run.lisp")
(main)
```

Other handy entry points:
- `(quick-demo)` — quick dead-battery demonstration
- `(run-demo-scenarios)` — menu of demo scenarios
- `(diagnose-car-problem)` — programmatic diagnosis flow (non-UI)

## Contradictions and Negative Evidence

The engine supports simple domain-level contradictions so that known evidence for one fact counts as negative evidence against its opposite during goal proving.

- Example contradictions in this domain:
  - `(lights work)` ↔ `(lights dim-or-off)`
  - `(car starts)` ↔ `(car does-not-start)`

- How it works:
  - If a rule requires a condition `C`, but a contradictory fact `C'` is already known with certainty `k`, the condition contributes negative evidence `-k` to the rule, which can produce a negative CF for the goal.

- How to extend:
  - Register additional contradictions in `car-rules.lisp` or from the REPL:
    - `(expert-system:register-contradiction '(engine running) '(car does-not-start))`

## Interactive Questions Toggle

You can toggle user prompts on/off to support scripted runs and automated testing:

- Programmatic control:
  - `(expert-system:disable-questions)` — non-interactive (unknowns treated as 0)
  - `(expert-system:enable-questions)` — re-enable prompts

- From the UI:
  - The consultation menu includes “Toggle interactive questions (ON/OFF)”.

## Tracing Toggle

Enable or disable detailed reasoning logs during a session:

- From the UI:
  - Choose “Toggle tracing (ON/OFF)” in the consultation menu.
- Programmatic control:
  - `(toggle-tracing)` to flip state
  - `(enable-trace)` / `(disable-trace)` for explicit control

## Notes

- Loading is centralized. Code that previously self-loaded dependencies has been simplified. Use `run.lisp` as the entry point; it loads the engine first, then the rules.
- The engine no longer `eval`s rule data. Rules are treated as data and matched structurally for correctness and safety.
- Performance benchmarks use SBCL-specific memory metrics when available and fall back gracefully on other Lisps.

If something seems off, check `../TROUBLESHOOTING.md` for a concise setup and debugging guide.

## 🔬 Testing

### Working Tests

- `sbcl --script test.lisp` – quick smoke test (dead battery & overheating scenarios)
- `sbcl --load certainty-factor-tests.lisp --eval "(in-package :expert-system)" --eval "(run-certainty-tests)" --quit`
- `sbcl --load comprehensive-tests.lisp --eval "(in-package :expert-system)" --eval "(run-comprehensive-tests)" --quit`
- `sbcl --eval "(require :asdf)" --eval "(asdf:test-system :car-expert-system)" --quit`

During automated runs the suites set `*interactive-questions*` to `nil` so that no prompts appear.

## 📈 Future Enhancements

### Code Quality Fixes:
- [x] Fix syntax errors in `run.lisp`
- [x] Improve function organization and ordering
- [ ] Resolve path dependencies in file loading
- [ ] Add proper error handling for file operations

### Feature Enhancements:
- [ ] Add more diagnostic rules
- [x] Implement backward chaining
- [x] Add certainty factor scoring
- [ ] Create web interface
- [ ] Add car make/model specific rules

## 🎓 Educational Value

This project demonstrates both:

### AI Concepts:
- **Expert System Architecture**: Facts, rules, inference engine
- **Forward Chaining Inference**: Automatic rule application
- **Pattern Matching**: Flexible fact matching with variables
- **Knowledge Representation**: Automotive domain knowledge

### Software Engineering Lessons:
- **File dependency management**
- **Error handling and debugging**
- **Modular vs. monolithic design trade-offs**
- **Testing and validation strategies**

## 📝 Development Notes

The modular backward‑chaining expert system in this folder is the primary implementation.

### Historical Reference

The legacy `simple-car-expert.lisp` in the project root is a forward‑chaining educational example kept for comparison and teaching purposes. New development should target the backward‑chaining engine in this folder.

---

**Built with Common Lisp and SBCL** 🚗💡  
*Lessons learned through real development challenges*
