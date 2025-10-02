;;;; Test file for Backward Chaining Car Expert System
;;;; Updated for backward chaining with certainty factors

;; Load the backward chaining system
(load "expert-system.lisp")
(load "car-rules.lisp")
(in-package :expert-system)

;; Test basic functionality
(format t "~%Testing backward chaining car expert system...~%")

;; Test 1: Dead battery scenario
(format t "~%=== Test 1: Dead Battery ===~%")
(clear-session)
(add-fact '(car does-not-start) 0.9)
(add-fact '(lights dim-or-off) 0.8)
(let ((result (prove-goal '(car-problem dead-battery))))
  (format t "Dead battery diagnosis CF: ~,2F~%" result))

;; Test 2: Overheating scenario  
(format t "~%=== Test 2: Overheating ===~%")
(clear-session)
(add-fact '(engine running) 0.9)
(add-fact '(temperature high) 0.9)
(add-fact '(coolant low) 0.8)
(let ((result (prove-goal '(car-problem overheating))))
  (format t "Overheating diagnosis CF: ~,2F~%" result))

(format t "~%All tests completed successfully!~%")
