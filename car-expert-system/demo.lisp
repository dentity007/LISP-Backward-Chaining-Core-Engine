;; =============================================================================
;; DEMO.LISP
;; Interactive Demonstrations for Car Expert System
;; =============================================================================

(load "car-rules.lisp")

(defun demo-menu ()
  "Interactive demo menu"
  (format t "~%")
  (format t "================================================~%")
  (format t "    CAR EXPERT SYSTEM - DEMO MODE~%")
  (format t "================================================~%")
  (format t "Choose a demonstration:~%")
  (format t "1. Dead Battery Scenario~%")
  (format t "2. Engine Overheating Scenario~%")
  (format t "3. Brake Problems Scenario~%")
  (format t "4. Transmission Issues Scenario~%")
  (format t "5. Interactive Diagnosis Mode~%")
  (format t "6. Show System Status~%")
  (format t "7. Exit~%")
  (format t "~%Enter your choice (1-7): "))

(defun run-demo ()
  "Main demo function"
  (loop
    (demo-menu)
    (let ((choice (read)))
      (case choice
        (1 (demo-dead-battery))
        (2 (demo-overheating))
        (3 (demo-brake-problems))
        (4 (demo-transmission))
        (5 (simple-diagnosis))
        (6 (system-status))
        (7 (format t "Thanks for using the Car Expert System!~%")
           (return))
        (otherwise (format t "Invalid choice. Please try again.~%"))))))

(defun demo-dead-battery ()
  "Demonstrate dead battery diagnosis"
  (format t "~%=== DEAD BATTERY SCENARIO ===~%")
  (format t "Symptoms: Car won't start, lights are dim~%")
  (reset-system)
  (enable-trace)
  (add-fact '(car does-not-start))
  (add-fact '(lights dim-or-off))
  (forward-chain)
  (show-diagnosis-results)
  (disable-trace))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== ENGINE OVERHEATING SCENARIO ===~%")
  (format t "Symptoms: Engine running hot, coolant low~%")
  (reset-system)
  (enable-trace)
  (add-fact '(engine running))
  (add-fact '(temperature high))
  (add-fact '(coolant low))
  (forward-chain)
  (show-diagnosis-results)
  (disable-trace))

(defun demo-brake-problems ()
  "Demonstrate brake problem diagnosis"
  (format t "~%=== BRAKE PROBLEMS SCENARIO ===~%")
  (format t "Symptoms: Brake warning light is on~%")
  (reset-system)
  (enable-trace)
  (add-fact '(brake-warning-light on))
  (forward-chain)
  (show-diagnosis-results)
  (disable-trace))

(defun demo-transmission ()
  "Demonstrate transmission problem diagnosis"
  (format t "~%=== TRANSMISSION ISSUES SCENARIO ===~%")
  (format t "Symptoms: Engine revs high, poor acceleration~%")
  (reset-system)
  (enable-trace)
  (add-fact '(car starts))
  (add-fact '(engine revs-high))
  (add-fact '(acceleration poor))
  (forward-chain)
  (show-diagnosis-results)
  (disable-trace))

(defun show-diagnosis-results ()
  "Show diagnosis and recommendations"
  (format t "~%--- DIAGNOSIS RESULTS ---~%")
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
          (format t "Possible Causes:~%")
          (dolist (diagnosis diagnoses)
            (format t "  • ~{~A~^ ~}~%" (rest diagnosis))))
        (format t "No specific diagnosis found.~%"))
    
    (if recommendations
        (progn
          (format t "~%Recommendations:~%")
          (dolist (recommendation recommendations)
            (format t "  • ~A~%" (second recommendation))))
        (format t "~%No specific recommendations.~%"))))

(defun simple-diagnosis ()
  "Simple interactive diagnosis"
  (format t "~%=== INTERACTIVE DIAGNOSIS ===~%")
  (reset-system)
  
  (format t "Does your car start? (y/n): ")
  (let ((starts (read)))
    (if (or (eq starts 'y) (eq starts 'yes))
        (add-fact '(car starts))
        (add-fact '(car does-not-start))))
  
  (when (fact-exists-p '(car does-not-start))
    (format t "Do the lights work? (y/n): ")
    (let ((lights (read)))
      (if (or (eq lights 'y) (eq lights 'yes))
          (add-fact '(lights work))
          (add-fact '(lights dim-or-off)))))
  
  (when (fact-exists-p '(car starts))
    (format t "Are there warning lights on dashboard? (y/n): ")
    (let ((warning (read)))
      (when (or (eq warning 'y) (eq warning 'yes))
        (add-fact '(dashboard warning-lights)))))
  
  (format t "~%Running diagnosis...~%")
  (forward-chain)
  (show-diagnosis-results))

;; Start the demo
(format t "~%Car Expert System Demo loaded!~%")
(format t "Type (run-demo) to start interactive demonstrations~%")
(format t "Type (simple-diagnosis) for quick diagnosis~%")
(format t "Type (system-status) to see system information~%")