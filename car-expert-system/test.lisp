;; Simple test file for car expert system
(load "car-rules.lisp")

;; Test basic functionality
(format t "~%Testing car expert system...~%")

;; Test 1: Dead battery scenario
(format t "~%=== Test 1: Dead Battery ===~%")
(reset-system)
(add-fact '(car does-not-start))
(add-fact '(lights dim-or-off))
(forward-chain)
(show-facts)

;; Test 2: Overheating scenario  
(format t "~%=== Test 2: Overheating ===~%")
(reset-system)
(add-fact '(engine running))
(add-fact '(temperature high))
(add-fact '(coolant low))
(forward-chain)
(show-facts)

(format t "~%All tests completed successfully!~%")