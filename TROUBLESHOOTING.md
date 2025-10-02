# TROUBLESHOOTING GUIDE - Backward Chaining Expert System

## 🚨 Quick Fixes for Common Issues

### Issue: SBCL Not Found
```bash
# Error: "sbcl: command not found"
# Solution:
brew install sbcl  # macOS
sudo apt install sbcl  # Ubuntu/Debian
```

### Issue: Package Errors
```bash
# Error: "Package EXPERT-SYSTEM does not exist"
# Solution: Load files in correct order
(load "car-expert-system/expert-system.lisp")  ; First
(load "car-expert-system/car-rules.lisp")      ; Second  
(load "car-expert-system/run.lisp")           ; Third
```

### Issue: VS Code LSP Errors
```json
// Error: "Path to shell executable '...cl-lsp' does not exist"
// Solution: Add to .vscode/settings.json
{
    "commonlisp.enableLSP": false,
    "commonlisp.client": "sbcl",
    "commonlisp.liveMode": false
}
```

### Issue: Alive Extension Problems
```bash
# Error: "Couldn't start client Alive Client"
# Solution: Use VS Code tasks instead
# Press Ctrl+Shift+P → "Tasks: Run Task" → "SBCL REPL"
```

### Issue: Rules Not Loading
```lisp
;; Error: "The function DEFINE-RULE is undefined"
;; Solution: Ensure correct package
(in-package :expert-system)
;; Check if rules loaded properly
(length *rules*)  ; Should show 20+ rules
```

### Issue: Certainty Factor Errors
```lisp
;; Error: "The variable *CERTAINTY-THRESHOLD* is unbound"  
;; Solution: Load expert-system.lisp first
(load "car-expert-system/expert-system.lisp")
(in-package :expert-system)
*certainty-threshold*  ; Should show 0.2
```

## ✅ Guaranteed Working Setup

### Method 1: Direct Loading (Recommended)
```bash
sbcl
* (load "car-expert-system/run.lisp")
* (main)
```

### Method 2: Step-by-Step Loading
```bash
sbcl
* (load "car-expert-system/expert-system.lisp")
* (load "car-expert-system/car-rules.lisp")  
* (in-package :expert-system)
* (diagnose-car-problem)
```

### Method 3: VS Code Integration  
```bash
# In VS Code:
# 1. Press Ctrl+Shift+P
# 2. Select "Tasks: Run Task"
# 3. Choose "SBCL REPL"
# 4. In REPL: (load "car-expert-system/run.lisp")
# 5. Run: (main)
```

## 🔧 Advanced Troubleshooting

### Debugging Rule Issues
```lisp
;; Check if rules are defined
(length *rules*)
(mapcar #'rule-name *rules*)

;; Test individual rule
(prove-goal '(car-problem dead-battery))

;; Enable tracing to see reasoning
(enable-trace)
(prove-goal '(car-problem dead-battery))
(disable-trace)
```

### Memory and Performance Issues
```lisp
;; Clear session between tests
(clear-session)

;; Check memory usage
(room)

;; Force garbage collection if needed
(sb-ext:gc)
```

### Package Conflicts
```lisp
;; If you get package conflicts, restart SBCL
;; Or explicitly switch packages:
(in-package :expert-system)

;; List all packages if confused
(list-all-packages)
```

### Custom Certainty Issues
```lisp
;; If certainty calculations seem wrong:
;; Test the combination function
(combine-certainty 0.8 0.6)  ; Should give ~0.92

;; Check threshold setting
*certainty-threshold*  ; Default: 0.2
(setf *certainty-threshold* 0.1)  ; More sensitive
```

## 🧪 Testing Your Installation

### Basic Functionality Test
```lisp
(load "car-expert-system/expert-system.lisp")
(in-package :expert-system)

;; Test certainty factor math
(combine-certainty 0.8 0.6)
;; Should return approximately 0.92

;; Test fact management
(clear-session)
(add-fact '(test-fact) 0.7)
(get-fact-cf '(test-fact))
;; Should return 0.7

;; Test rule definition
(define-rule test-rule
  '(conclusion test)
  '((test-fact))
  0.9)
;; Should add rule successfully
```

### Full System Test
```lisp
(load "car-expert-system/run.lisp")
(quick-demo)
;; Should run demonstration without errors
```

## 📝 Error Message Dictionary

| Error Message | Meaning | Solution |
|--------------|---------|----------|
| "Package X does not exist" | Wrong package or files not loaded | Load expert-system.lisp first |
| "Function X is undefined" | Missing function definition | Check file loading order |
| "Variable X is unbound" | Variable not initialized | Ensure expert-system.lisp loaded |
| "Maximum debugger nesting" | Error in error handler | Restart SBCL completely |
| "Can't find rule" | Rule not defined | Check rule definitions in car-rules.lisp |
| "Invalid certainty factor" | CF outside -1 to 1 range | Use values between -1 and 1 |

## 🆘 Emergency Recovery

### If Everything Breaks:
1. **Restart SBCL completely**
2. **Load only essential files:**
   ```lisp
   (load "car-expert-system/expert-system.lisp")
   (in-package :expert-system)
   ```
3. **Test basic functionality:**
   ```lisp
   (clear-session)
   (add-fact '(car does-not-start) 0.9)
   (show-facts)
   ```

### If VS Code Issues Persist:
1. **Use terminal SBCL instead**
2. **Disable problematic extensions**
3. **Reset VS Code settings:**
   ```bash
   rm -rf .vscode/settings.json
   # Restart VS Code
   ```

### Nuclear Option:
```bash
# Complete reinstall
brew uninstall sbcl
brew install sbcl
# Clone fresh repository
git clone https://github.com/dentity007/LISP-Backward-Chaining-Core-Engine.git
```

## 📞 Getting Help

If these solutions don't work:

1. **Check the GitHub Issues:** Look for similar problems
2. **Create an Issue:** Include error messages and system info
3. **SBCL Community:** The Common Lisp community is very helpful
4. **Stack Overflow:** Tag questions with "common-lisp" and "sbcl"

### Useful Diagnostic Commands:
```lisp
;; System information
(lisp-implementation-type)
(lisp-implementation-version)

;; Current package
*package*

;; Available functions
(apropos "PROVE")
(apropos "CERTAINTY")
```

---

**Remember:** The backward chaining system is more complex than forward chaining, but it's also more powerful. Most issues stem from package management or incorrect loading order. When in doubt, restart SBCL and load files step by step! 🚀

## 🎯 If Nothing Else Works

1. `cd` to project root directory
2. `sbcl`
3. `(load "simple-car-expert.lisp")`
4. `(demo-dead-battery)`

**This is guaranteed to work!**

---
*Copy this file to your desktop for quick reference during development*
