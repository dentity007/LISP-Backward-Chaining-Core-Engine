;; =============================================================================
;; SIMPLE-DEMO.LISP
;; Simple Car Expert System Demo with Better Path Handling
;; =============================================================================

;; Change to correct directory first
(defun load-car-system ()
  "Load the car expert system safely"
  (let ((current-dir (truename ".")))
    (format t "Current directory: ~A~%" current-dir)
    ;; Try to load from current directory first
    (handler-case
        (progn
          (load "expert-system.lisp")
          (format t "Loaded expert-system.lisp successfully~%"))
      (error (e)
        (format t "Error loading expert-system.lisp: ~A~%" e)
        (return-from load-car-system nil)))
    
    ;; Load car rules
    (handler-case
        (progn
          (load "car-rules.lisp")
          (format t "Loaded car-rules.lisp successfully~%"))
      (error (e)
        (format t "Error loading car-rules.lisp: ~A~%" e)
        (return-from load-car-system nil)))
    
    (format t "Car expert system loaded successfully!~%")
    t))

;; Quick demo functions
(defun quick-battery-test ()
  "Quick test of dead battery scenario"
  (format t "~%=== QUICK BATTERY TEST ===~%")
  (reset-system)
  (add-fact '(car does-not-start))
  (add-fact '(lights dim-or-off))
  (forward-chain)
  (format t "~%Results:~%")
  (show-facts))

(defun quick-overheating-test ()
  "Quick test of overheating scenario"
  (format t "~%=== QUICK OVERHEATING TEST ===~%")
  (reset-system)
  (add-fact '(engine running))
  (add-fact '(temperature high))
  (add-fact '(coolant low))
  (forward-chain)
  (format t "~%Results:~%")
  (show-facts))

(defun simple-interactive ()
  "Simple interactive diagnosis"
  (format t "~%=== SIMPLE CAR DIAGNOSIS ===~%")
  (reset-system)
  
  (format t "Answer with Y or N:~%")
  
  (format t "Does your car start? ")
  (force-output)
  (let ((answer (string-upcase (string-trim " " (read-line)))))
    (if (member answer '("Y" "YES") :test #'string=)
        (add-fact '(car starts))
        (add-fact '(car does-not-start))))
  
  (when (fact-exists-p '(car does-not-start))
    (format t "Do the headlights work normally? ")
    (force-output)
    (let ((answer (string-upcase (string-trim " " (read-line)))))
      (if (member answer '("Y" "YES") :test #'string=)
          (add-fact '(lights work))
          (add-fact '(lights dim-or-off)))))
  
  (format t "~%Running diagnosis...~%")
  (forward-chain)
  
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
        (format t "~%No specific recommendations.~%"))))

;; Try to load the system automatically
(format t "~%Attempting to load car expert system...~%")
(if (load-car-system)
    (progn
      (format t "~%=== CAR EXPERT SYSTEM READY ===~%")
      (format t "Available commands:~%")
      (format t "  (quick-battery-test)     - Test dead battery scenario~%")
      (format t "  (quick-overheating-test) - Test overheating scenario~%")
      (format t "  (simple-interactive)     - Interactive diagnosis~%")
      (format t "  (system-status)          - Show system information~%")
      (format t "~%Type one of the commands above to start!~%"))
    (format t "~%Failed to load system. Make sure you're in the car-expert-system directory.~%"))