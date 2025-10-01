# Car Troubleshooting Expert System

A Common Lisp implementation of an expert system for diagnosing car problems using forward chaining inference.

## 🚗 Project Overview

This expert system helps diagnose common car problems by asking questions about symptoms and applying a knowledge base of automotive troubleshooting rules. It uses forward chaining inference to determine possible causes and recommend solutions.

## 📁 Project Structure

```
car-expert-system/
├── expert-system.lisp    # Core expert system engine
├── car-rules.lisp       # Car troubleshooting knowledge base  
├── run.lisp            # Main interactive application (⚠️ has syntax issues)
├── demo.lisp           # Interactive demonstrations (⚠️ path dependency issues)
├── test.lisp           # Basic functionality tests (✅ works)
└── README.md           # This file
```

## ⚠️ IMPORTANT: Known Issues & Recommended Usage

### � **Files with Issues:**
- **`run.lisp`** - Has syntax errors in the `provide-final-diagnosis` function
- **`demo.lisp`** - Has file path dependency issues when loaded from parent directory

### ✅ **Recommended Approach:**
**Use the parent directory's `simple-car-expert.lisp` file instead!**

```lisp
# From parent directory
(load "simple-car-expert.lisp")  # Self-contained, no issues
```

## �🛠️ System Requirements

- **SBCL** (Steel Bank Common Lisp) 2.5.9 or later
- **VS Code** with Lisp extensions (already configured)

## 🚀 How to Use (Working Methods)

### Method 1: Self-Contained Version (RECOMMENDED)

```bash
# Navigate to parent directory
cd "/Users/nmaine/local copy github/LISP Auto Project"

# Start SBCL
sbcl

# Load working version
(load "simple-car-expert.lisp")

# Run demos
(demo-dead-battery)
(interactive-diagnosis)
```

### Method 2: Script Testing (Limited)

```bash
# Test basic engine functionality only
sbcl --script test.lisp
```

### Method 3: Manual Loading (Advanced)

If you want to use the modular files despite the issues:

```lisp
# Load in correct order
(load "expert-system.lisp")
# Then manually add rules instead of loading car-rules.lisp
```

## 🧠 Expert System Features

### Core Engine (`expert-system.lisp`) ✅
- **Fact Management**: Add, remove, and query facts
- **Rule Management**: Define and manage inference rules
- **Pattern Matching**: Match patterns with variables
- **Forward Chaining**: Automatic inference engine
- **Tracing**: Debug mode to see inference process

### Knowledge Base (`car-rules.lisp`) ⚠️
- **20+ diagnostic rules** covering:
  - Starting problems (dead battery, starter issues)
  - Engine performance (overheating, rough idle)
  - Transmission problems (slipping, hard shifting)
  - Brake issues (worn pads, fluid leaks)
  - Electrical problems (charging system)
  - Tire and suspension issues

**Issue:** File tries to load `expert-system.lisp` with relative path, causing errors when loaded from parent directory.

### Example Rules
```lisp
;; Dead battery diagnosis
(add-rule 'dead-battery-rule
          '((car does-not-start) (lights dim-or-off))
          '((diagnosis dead battery) 
            (recommend "Check battery voltage and connections"))
          "Car won't start and lights are dim")
```

## 📋 Lessons Learned & Development Issues

### Issue 1: File Path Dependencies
**Problem:** `car-rules.lisp` contains `(load "expert-system.lisp")` which fails when loaded from parent directory.

**Solutions:**
- Use absolute paths: `(load "/full/path/expert-system.lisp")`
- Create self-contained files (like `simple-car-expert.lisp`)
- Use proper directory context handling

### Issue 2: Function Definition Order
**Problem:** Functions called before they're defined cause compilation warnings.

**Solutions:**
- Define helper functions before main functions
- Use forward declarations when necessary
- Organize code with dependencies in mind

### Issue 3: Structure Redefinition
**Problem:** Loading files multiple times causes structure conflicts.

**Solutions:**
- Restart REPL for clean state
- Use debugger restart options (0 or 1)
- Design for single-load scenarios

### Issue 4: Syntax Errors in Complex Functions
**Problem:** `run.lisp` has unmatched parentheses in `provide-final-diagnosis`.

**Root Cause:** Complex nested conditionals with incorrect parenthesis matching.

**Prevention:**
- Use proper Lisp editors with parenthesis matching
- Break complex functions into smaller parts
- Test incrementally during development

## 🎯 Demo Scenarios (Working Examples)

### Available Demonstrations
1. **Dead Battery**: Car won't start, lights dim
2. **Overheating**: Engine running hot, coolant low
3. **Brake Problems**: Warning lights, safety issues
4. **Transmission**: Poor acceleration, slipping

### Sample Output (from working simple-car-expert.lisp)
```
=== DEAD BATTERY DEMO ===
System reset.
Applied rule DEAD-BATTERY: Added (DIAGNOSIS DEAD-BATTERY)
Applied rule DEAD-BATTERY: Added (RECOMMEND Check battery connections)
Applied rule DEAD-BATTERY: Added (RECOMMEND Jump start car)

=== RESULTS ===
Diagnosis:
  • DEAD-BATTERY

Recommendations:
  • Jump start car
  • Check battery connections
```

## 🔧 System Functions

### Basic Operations (from working system)
```lisp
(reset-system)          ; Clear all facts and reset rules
(show-facts)           ; List current facts
(load-car-rules)       ; Load diagnostic rules
(apply-rules)          ; Run inference engine
```

### Manual Fact Management
```lisp
(add-fact '(car does-not-start))
(add-fact '(lights dim-or-off))
(apply-rules)          ; Run inference
```

## 🐛 Known Issues & Workarounds

### Issues in This Directory:
1. **`run.lisp`**: Syntax errors prevent loading
   - **Workaround**: Use `../simple-car-expert.lisp` instead

2. **`demo.lisp`**: Path dependency on `car-rules.lisp`
   - **Workaround**: Load from car-expert-system directory or use parent's version

3. **`car-rules.lisp`**: Relative path loading issues
   - **Workaround**: Load `expert-system.lisp` first manually

### General Issues:
4. **Function ordering**: Warnings about undefined functions
   - **Impact**: Warnings only, system usually works
   - **Fix**: Reorder function definitions

5. **Structure conflicts**: When reloading files
   - **Fix**: Restart REPL or use debugger restarts

## 🔬 Testing

### Working Tests:
```bash
# Basic functionality (works)
sbcl --script test.lisp

# Expected output shows successful diagnosis of:
# - Dead battery scenarios  
# - Overheating problems
```

### Recommended Testing:
```bash
# Use parent directory's working version
cd ..
sbcl
(load "simple-car-expert.lisp")
(demo-dead-battery)
```

## 📈 Future Enhancements

### Code Quality Fixes:
- [ ] Fix syntax errors in `run.lisp`
- [ ] Resolve path dependencies in file loading
- [ ] Improve function organization and ordering
- [ ] Add proper error handling for file operations

### Feature Enhancements:
- [ ] Add more diagnostic rules
- [ ] Implement backward chaining
- [ ] Add probability/confidence scoring
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

## 🏁 Quick Start (Actual Working Version)

```bash
# Navigate to parent directory
cd ..

# Start SBCL REPL
sbcl

# Load working version
(load "simple-car-expert.lisp")

# Try it out
(demo-dead-battery)
(interactive-diagnosis)
```

## 📝 Development Notes

This modular version serves as a learning example of both expert system design and the importance of proper file organization in software development. While some files have issues, they demonstrate real-world development challenges and their solutions.

**For actual use, prefer the working `simple-car-expert.lisp` in the parent directory.**

---

**Built with Common Lisp and SBCL** 🚗💡  
*Lessons learned through real development challenges*