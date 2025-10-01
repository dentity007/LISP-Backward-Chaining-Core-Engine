;; =============================================================================
;; SIMPLE-DEMO.LISP
;; Simple Backward Chaining Car Expert System Demo
;; Updated for goal-driven reasoning with certainty factors
;; =============================================================================

;; Load the backward chaining system
(load "expert-system.lisp")
(load "car-rules.lisp")
(in-package :expert-system)

;; Quick demo functions using backward chaining
(defun quick-battery-test ()
  "Quick test of dead battery scenario with backward chaining"
  (format t "~%=== QUICK BATTERY TEST (Backward Chaining) ===~%")
  (clear-facts)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (let ((result (prove-goal '(car-problem dead-battery))))
    (format t "Dead battery diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Dead battery is LIKELY~%")
        (format t "CONCLUSION: Dead battery is UNLIKELY~%"))))

(defun quick-overheating-test ()
  "Quick test of overheating scenario with backward chaining"
  (format t "~%=== QUICK OVERHEATING TEST (Backward Chaining) ===~%")
  (clear-facts)
  (add-fact '(engine running) 0.9)
  (add-fact '(temperature high) 0.9)
  (add-fact '(coolant low) 0.8)
  (let ((result (prove-goal '(car-problem overheating))))
    (format t "Overheating diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Overheating is LIKELY~%")
        (format t "CONCLUSION: Overheating is UNLIKELY~%"))))

(defun quick-starter-test ()
  "Quick test of starter failure scenario"
  (format t "~%=== QUICK STARTER TEST (Backward Chaining) ===~%")
  (clear-facts)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights work) 0.9)
  (add-fact '(clicking-sound) 0.8)
  (let ((result (prove-goal '(car-problem starter-failure))))
    (format t "Starter failure diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Starter failure is LIKELY~%")
        (format t "CONCLUSION: Starter failure is UNLIKELY~%"))))

(defun simple-interactive ()
  "Simple interactive diagnosis using backward chaining"
  (format t "~%=== SIMPLE CAR DIAGNOSIS (Backward Chaining) ===~%")
  (clear-facts)
  
  (format t "Answer with Y or N:~%")
  
  (format t "Does your car start? ")
  (force-output)
  (let ((answer (string-upcase (string-trim " " (read-line)))))
    (if (member answer '("Y" "YES") :test #'string=)
        (add-fact '(car starts) 0.9)
        (add-fact '(car does-not-start) 0.9)))
  
  (when (fact-known-p '(car does-not-start))
    (format t "Do the headlights work normally? ")
    (force-output)
    (let ((answer (string-upcase (string-trim " " (read-line)))))
      (if (member answer '("Y" "YES") :test #'string=)
          (add-fact '(lights work) 0.8)
          (add-fact '(lights dim-or-off) 0.8))))
  
  (format t "~%Running backward chaining diagnosis...~%")
  
  ;; Test multiple car problems
  (let ((problems '((car-problem dead-battery)
                    (car-problem starter-failure)
                    (car-problem fuel-system)
                    (car-problem ignition-system)
                    (car-problem overheating))))
    
    (format t "~%=== DIAGNOSIS RESULTS ===~%")
    (let ((found-problems nil))
      (dolist (problem problems)
        (let ((cf (prove-goal problem)))
          (when (> (abs cf) 0.1)  ; Only show problems with some evidence
            (push (list problem cf) found-problems))))
      
      (if found-problems
          (progn
            (format t "Possible problems (ranked by confidence):~%")
            (setf found-problems (sort found-problems (lambda (a b) (> (second a) (second b)))))
            (dolist (problem-result found-problems)
              (let ((problem (first problem-result))
                    (cf (second problem-result)))
                (format t "  • ~A: ~,1F% confidence~%" 
                        (case (second problem)
                          (dead-battery "Dead Battery")
                          (starter-failure "Starter Failure") 
                          (fuel-system "Fuel System")
                          (ignition-system "Ignition System")
                          (overheating "Overheating")
                          (otherwise "Unknown Problem"))
                        (* (abs cf) 100)))))
          (format t "No specific diagnosis found with current evidence.~%")))))