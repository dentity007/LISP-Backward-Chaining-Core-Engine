;;;; Debug test for demo functions
(load "car-rules.lisp")
(in-package :expert-system)

;; Test direct calls
(format t "=== Direct Test ===~%")
(clear-facts)
(add-fact '(car does-not-start) 0.9)
(add-fact '(lights dim-or-off) 0.8)
(format t "Facts added~%")
(let ((result (prove-goal '(car-problem dead-battery))))
  (format t "Direct prove-goal result: ~,2F~%" result))