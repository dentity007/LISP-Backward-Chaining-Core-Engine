# TROUBLESHOOTING QUICK REFERENCE

## 🚨 Common Issues & Instant Solutions

### Issue: "Path to shell executable '/Users/username/.roswell/bin/cl-lsp' does not exist"

**Instant Fix:**
```json
// In .vscode/settings.json
{
    "commonlisp.enableLSP": false
}
```

### Issue: "Couldn't start client Alive Client"

**Instant Fix:**
Use VS Code tasks instead:
`Cmd+Shift+P` → "Tasks: Run Task" → "Start SBCL REPL"

### Issue: File loading errors / "file does not exist"

**Instant Fix:**
```lisp
# Use the working self-contained version
(load "simple-car-expert.lisp")
```

### Issue: Structure redefinition warnings

**Instant Fix:**
```lisp
# In debugger, choose restart option:
0  # or type (sb-ext:exit) and restart SBCL
```

### Issue: Syntax errors in run.lisp or demo.lisp

**Instant Fix:**
```lisp
# Skip problematic files, use working version:
(load "simple-car-expert.lisp")
(demo-dead-battery)
```

## ✅ Guaranteed Working Commands

```lisp
# This always works:
sbcl
(load "simple-car-expert.lisp")
(demo-dead-battery)
(interactive-diagnosis)
```

## 🎯 If Nothing Else Works

1. `cd` to project root directory
2. `sbcl`
3. `(load "simple-car-expert.lisp")`
4. `(demo-dead-battery)`

**This is guaranteed to work!**

---
*Copy this file to your desktop for quick reference during development*