# LISP Backward Chaining Core Engine – Car Diagnostic Expert System

A Common Lisp expert system that diagnoses automotive problems using goal-driven backward chaining and MYCIN-style certainty factors. The engine asks only the questions it needs, combines evidence with confidence scores, and reports conclusions together with actionable recommendations.

## Why This Project Stands Out
- 🎯 **Goal-driven reasoning** – works backwards from hypotheses instead of flooding users with every question
- 📊 **MYCIN certainty factors** – propagates confidence values between -1.0 and 1.0 for nuanced results
- 🤖 **Interactive consultation** – dynamically generated prompts with traceable inference steps
- 🔍 **Explainable outcomes** – recommendations and safety notices accompany each likely diagnosis
- 🧰 **Educational focus** – showcases modern Common Lisp techniques for AI, knowledge representation, and tooling

## System Architecture
| Layer | File(s) | Responsibilities |
|-------|---------|------------------|
| Inference Engine | `car-expert-system/expert-system.lisp` | Fact store, backward chaining, certainty math, tracing, question control |
| Knowledge Base | `car-expert-system/car-rules.lisp` | Automotive rules, contradictions, recommendations, high-level goals |
| User Interface | `car-expert-system/run.lisp` | Interactive console menu, demo scenarios, reporting helpers |

Supporting files include automated test suites, performance demos, and legacy forward-chaining examples for comparison.

## 🚀 Quick Start
### Option 1 – SBCL REPL (recommended)
```bash
git clone https://github.com/dentity007/LISP-Backward-Chaining-Core-Engine.git
cd LISP-Backward-Chaining-Core-Engine
sbcl
* (load "car-expert-system/run.lisp")
* (main)
```

### Option 2 – VS Code Tasks
1. Open the project folder in VS Code.
2. `Cmd/Ctrl+Shift+P` → **Tasks: Run Task** → `SBCL REPL` (defined in `.vscode/tasks.json`).
3. From the REPL run `(load "car-expert-system/run.lisp")` then `(main)`.

## Running the Expert System
- `(main)` launches the full consultation and follow-up menu.
- `(run-comprehensive-diagnosis)` checks every major problem without the banner/menu.
- `(quick-demo)` seeds a dead-battery scenario and prints the certainty result.
- `(run-demo-scenarios)` walks through curated cases (dead battery, overheating, brake failure, mixed symptoms).
- Toggle behaviour at runtime:
  - `(toggle-questions)` / `(disable-questions)` / `(enable-questions)` control whether unknown facts prompt the user.
  - `(toggle-tracing)` / `(enable-trace)` / `(disable-trace)` show reasoning steps for each rule.
  - `(show-facts)` lists all facts collected so far with their certainty factors.

## Testing
| Purpose | Command |
|---------|---------|
| Quick smoke run (dead battery & overheating) | `sbcl --script car-expert-system/test.lisp` |
| Certainty-factor unit tests | `sbcl --load car-expert-system/certainty-factor-tests.lisp --eval "(in-package :expert-system)" --eval "(run-certainty-tests)" --quit` |
| Comprehensive scenario tests (non-interactive) | `sbcl --load car-expert-system/comprehensive-tests.lisp --eval "(in-package :expert-system)" --eval "(run-comprehensive-tests)" --quit` |
| ASDF-integrated test suite | `sbcl --eval "(require :asdf)" --eval "(asdf:test-system :car-expert-system)" --quit` |

During automated runs the suites disable interactive questions; mimic this behaviour manually with `(expert-system:disable-questions)` if you need deterministic scripts.

## Project Layout
```
.
├── README.md                     # Project overview (this file)
├── TROUBLESHOOTING.md            # Setup and debugging guide
├── car-expert-system/
│   ├── expert-system.lisp        # Core backward-chaining engine
│   ├── car-rules.lisp            # Knowledge base & contradictions
│   ├── run.lisp                  # Interactive UI and demos
│   ├── test.lisp                 # Smoke tests
│   ├── comprehensive-tests.lisp  # Scenario assertions
│   └── certainty-factor-tests.lisp # Unit tests for CF math
├── car-expert-system.asd         # ASDF system + test definitions
├── simple-car-expert.lisp        # Legacy forward-chaining example
├── load-system.lisp              # Legacy loader (historical)
├── hello.lisp / quick-test.lisp  # Environment sanity checks
└── .vscode/                      # Editor tasks and settings
```

## Development Tips
- Re-run `(clear-session)` between experiments to wipe the fact store and asked-question cache.
- Use `(enable-trace)` before calling `(prove-goal ...)` or `(main)` to follow rule selection and certainty math.
- Register additional contradictions with `(register-contradiction fact-a fact-b)` to model mutually exclusive evidence.
- Adjust sensitivity through `(setf *certainty-threshold* 0.3)` (defaults to 0.2).

## Legacy Forward-Chaining Demo (Optional)
The original forward-chaining expert system remains for historical comparison:
```bash
sbcl
* (load "simple-car-expert.lisp")
* (demo-dead-battery)
```
This version applies rules in a data-driven order and does not use certainty factors.

## Troubleshooting & Support
- The dedicated guide in `TROUBLESHOOTING.md` covers SBCL setup, VS Code integration, LSP quirks, and recovery steps.
- If issues persist, open a GitHub issue with SBCL version, operating system, and the steps you attempted.

## Roadmap
- [ ] Expand the automotive knowledge base with additional subsystems and questions
- [ ] Improve file-loading portability and user-friendly error reporting
- [ ] Explore a small web or GUI wrapper for broader accessibility
- [x] Deliver backward chaining with certainty factors and comprehensive documentation

Enjoy exploring AI reasoning in Common Lisp! 🚗💡
