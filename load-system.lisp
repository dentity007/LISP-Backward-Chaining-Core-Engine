;; =============================================================================
;; LOAD-SYSTEM.LISP
;; Safe loader for car expert system from any directory
;; =============================================================================

(defun load-car-expert-system ()
  "Load the car expert system with proper path handling"
  (let ((base-path "car-expert-system/"))
    (format t "Loading car expert system...~%")
    
    ;; Load expert system engine
    (handler-case
        (progn
          (load (concatenate 'string base-path "expert-system.lisp"))
          (format t "✓ Expert system engine loaded~%"))
      (error (e)
        (format t "✗ Error loading expert-system.lisp: ~A~%" e)
        (return-from load-car-expert-system nil)))
    
    ;; Load car rules (which also loads expert-system.lisp again, but that's OK)
    (handler-case
        (progn
          (load (concatenate 'string base-path "car-rules.lisp"))
          (format t "✓ Car rules loaded~%"))
      (error (e)
        (format t "✗ Error loading car-rules.lisp: ~A~%" e)
        (return-from load-car-expert-system nil)))
    
    (format t "✓ Car expert system ready!~%")
    t))

;; Demo functions that work after loading
(defun quick-demo ()
  "Quick demonstration of the expert system"
  (format t "~%=== QUICK DEMO ===~%")
  
  ;; Test 1: Dead battery
  (format t "~%Test 1: Dead Battery Scenario~%")
  (reset-system)
  (add-fact '(car does-not-start))
  (add-fact '(lights dim-or-off))
  (forward-chain)
  
  (let ((diagnoses (remove-if-not 
                    (lambda (fact) 
                      (and (listp fact) 
                           (eq (first fact) 'diagnosis)))
                    *facts*)))
    (format t "Diagnosis: ~{~A~^ ~}~%" (rest (first diagnoses))))
  
  ;; Test 2: Overheating
  (format t "~%Test 2: Overheating Scenario~%")
  (reset-system)
  (add-fact '(engine running))
  (add-fact '(temperature high))
  (add-fact '(coolant low))
  (forward-chain)
  
  (let ((diagnoses (remove-if-not 
                    (lambda (fact) 
                      (and (listp fact) 
                           (eq (first fact) 'diagnosis)))
                    *facts*)))
    (format t "Diagnosis: ~{~A~^ ~}~%" (rest (first diagnoses))))
  
  (format t "~%Demo completed!~%"))

(defun start-diagnosis ()
  "Start an interactive diagnosis session"
  (format t "~%=== CAR DIAGNOSIS ===~%")
  (reset-system)
  
  (format t "I'll help diagnose your car problem.~%")
  (format t "Answer with Y or N:~%~%")
  
  ;; Basic questions
  (format t "Does your car start? ")
  (finish-output)
  (let ((answer (string-upcase (string-trim " " (read-line)))))
    (if (member answer '("Y" "YES") :test #'string=)
        (progn
          (add-fact '(car starts))
          (format t "Car starts - checking other symptoms...~%"))
        (progn
          (add-fact '(car does-not-start))
          (format t "Car doesn't start - checking electrical system...~%")
          
          (format t "Do the headlights work normally? ")
          (finish-output)
          (let ((lights (string-upcase (string-trim " " (read-line)))))
            (if (member lights '("Y" "YES") :test #'string=)
                (add-fact '(lights work))
                (add-fact '(lights dim-or-off)))))))
  
  ;; Warning lights for cars that start
  (when (fact-exists-p '(car starts))
    (format t "Are any warning lights on the dashboard? ")
    (finish-output)
    (let ((warning (string-upcase (string-trim " " (read-line)))))
      (when (member warning '("Y" "YES") :test #'string=)
        (add-fact '(dashboard warning-lights)))))
  
  ;; Run inference
  (format t "~%Analyzing symptoms...~%")
  (forward-chain)
  
  ;; Show results
  (format t "~%=== DIAGNOSIS RESULTS ===~%")
  (let ((diagnoses (remove-if-not 
                    (lambda (fact) 
                      (and (listp fact) 
                           (eq (first fact) 'diagnosis)))
                    *facts*))
        (recommendations (remove-if-not 
                         (lambda (fact) 
                           (and (listp fact) 
                                (eq (first fact) 'recommend)))
                         *facts*)))
    
    (if diagnoses
        (progn
          (format t "Possible problems:~%")
          (dolist (diagnosis diagnoses)
            (format t "  • ~{~A~^ ~}~%" (rest diagnosis))))
        (format t "No specific diagnosis found.~%"))
    
    (if recommendations
        (progn
          (format t "~%Recommended actions:~%")
          (dolist (recommendation recommendations)
            (format t "  • ~A~%" (second recommendation))))
        (format t "~%Consult a qualified mechanic.~%"))))

;; Load the system automatically
(format t "~%")
(format t "================================================~%")
(format t "    CAR EXPERT SYSTEM LOADER~%")
(format t "================================================~%")

(if (load-car-expert-system)
    (progn
      (format t "~%Available commands:~%")
      (format t "  (quick-demo)        - Run quick demonstrations~%")
      (format t "  (start-diagnosis)   - Interactive car diagnosis~%")
      (format t "  (system-status)     - Show system information~%")
      (format t "  (show-facts)        - Show current facts~%")
      (format t "  (show-rules)        - Show all rules~%")
      (format t "~%Type a command to begin!~%"))
    (format t "~%System failed to load. Check file paths.~%"))