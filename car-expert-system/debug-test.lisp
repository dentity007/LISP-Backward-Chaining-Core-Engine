;; Simple debug test for backward chaining
(load "expert-system.lisp")
(load "car-rules.lisp")

(in-package :expert-system)

;; Test facts storage
(format t "=== Testing Facts Storage ===~%")
(clear-facts)
(add-fact '(car does-not-start) 0.9)
(add-fact '(lights dim-or-off) 0.8)
(format t "Number of facts stored: ~A~%" (hash-table-count *facts*))

;; Test rule lookup
(format t "~%=== Testing Rule Lookup ===~%")
(format t "Number of rules: ~A~%" (length *rules*))
(format t "First rule goal: ~A~%" (if *rules* (rule-goal (first *rules*)) "No rules"))
(format t "Looking for dead-battery rules:~%")
(loop for rule in *rules*
      when (equal (rule-goal rule) ''(car-problem dead-battery))
      do (format t "  Found dead-battery rule: ~A~%" (rule-name rule)))
(format t "First 5 rule goals:~%")
(loop for rule in (subseq *rules* 0 (min 5 (length *rules*)))
      do (format t "  ~A~%" (rule-goal rule)))

;; Test simple fact retrieval
(format t "~%=== Testing Fact Retrieval ===~%")
(let ((cf1 (get-fact-cf '(car does-not-start)))
      (cf2 (get-fact-cf '(lights dim-or-off))))
  (format t "CF for (car does-not-start): ~A~%" cf1)
  (format t "CF for (lights dim-or-off): ~A~%" cf2))

;; Test goal proving with detailed tracing
(format t "~%=== Testing Goal Proving with Detailed Trace ===~%")
(enable-trace)

;; First, let's check the dead-battery rule details
(let ((dead-battery-rule (find-if (lambda (rule) 
                                    (equal (rule-goal rule) ''(car-problem dead-battery)))
                                  *rules*)))
  (when dead-battery-rule
    (format t "Dead-battery rule found:~%")
    (format t "  Name: ~A~%" (rule-name dead-battery-rule))
    (format t "  Goal: ~A~%" (rule-goal dead-battery-rule))
    (format t "  Conditions: ~A~%" (rule-conditions dead-battery-rule))
    (format t "  CF: ~A~%" (rule-cf dead-battery-rule))
    (format t "  Question: ~A~%" (rule-question dead-battery-rule))
    
    ;; Test the goal matching issue
    (format t "~%Goal matching test:~%")
    (format t "  Rule goal: ~A~%" (rule-goal dead-battery-rule))
    (format t "  Test goal: ~A~%" '(car-problem dead-battery))
    (format t "  Direct equal: ~A~%" (equal (rule-goal dead-battery-rule) '(car-problem dead-battery)))
    (format t "  Equal after eval: ~A~%" (equal (eval (rule-goal dead-battery-rule)) '(car-problem dead-battery)))
    
    ;; Test condition matching
    (format t "~%Condition matching test:~%")
    (format t "  Rule conditions: ~A~%" (rule-conditions dead-battery-rule))
    (format t "  Eval conditions: ~A~%" (eval (rule-conditions dead-battery-rule)))
    (format t "  First condition: ~A~%" (first (eval (rule-conditions dead-battery-rule))))))

;; Now test the main goal after the fix
(format t "~%Testing main goal after fix:~%")
(let ((result (prove-goal '(car-problem dead-battery))))
  (format t "Final result for (car-problem dead-battery): ~A~%" result))