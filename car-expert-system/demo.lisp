;; =============================================================================
;; DEMO.LISP
;; Interactive Demonstrations for Backward Chaining Car Expert System
;; Updated for goal-driven reasoning with certainty factors
;; =============================================================================

(load "expert-system.lisp")
(load "car-rules.lisp")
(in-package :expert-system)

(defun demo-menu ()
  "Interactive demo menu for backward chaining system"
  (format t "~%")
  (format t "================================================~%")
  (format t "    BACKWARD CHAINING CAR EXPERT SYSTEM~%")
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
  (clear-facts)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (let ((result (prove-goal '(car-problem dead-battery))))
    (format t "Dead battery diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Dead battery is LIKELY~%")
        (format t "CONCLUSION: Dead battery is UNLIKELY~%"))))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== ENGINE OVERHEATING SCENARIO ===~%")
  (format t "Symptoms: Engine running hot, coolant low~%")
  (clear-facts)
  (add-fact '(engine running) 0.9)
  (add-fact '(temperature high) 0.9)
  (add-fact '(coolant low) 0.8)
  (let ((result (prove-goal '(car-problem overheating))))
    (format t "Overheating diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Overheating is LIKELY~%")
        (format t "CONCLUSION: Overheating is UNLIKELY~%"))))

(defun demo-brake-problems ()
  "Demonstrate brake problem diagnosis"
  (format t "~%=== BRAKE PROBLEMS SCENARIO ===~%")
  (format t "Symptoms: Brake warning light is on~%")
  (clear-facts)
  (add-fact '(brake-warning-light on) 0.9)
  (let ((result (prove-goal '(car-problem brake-system))))
    (format t "Brake system diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Brake system problem is LIKELY~%")
        (format t "CONCLUSION: Brake system problem is UNLIKELY~%"))))

(defun demo-transmission ()
  "Demonstrate transmission problem diagnosis"
  (format t "~%=== TRANSMISSION ISSUES SCENARIO ===~%")
  (format t "Symptoms: Engine revs high, poor acceleration~%")
  (clear-facts)
  (add-fact '(car starts) 0.9)
  (add-fact '(engine revs-high) 0.8)
  (add-fact '(acceleration poor) 0.8)
  (let ((result (prove-goal '(car-problem transmission-slip))))
    (format t "Transmission diagnosis CF: ~,2F (~,1F% confidence)~%" result (* result 100))
    (if (certainty-true-p result)
        (format t "CONCLUSION: Transmission problem is LIKELY~%")
        (format t "CONCLUSION: Transmission problem is UNLIKELY~%"))))

(defun show-diagnosis-results ()
  "Show diagnosis results using backward chaining"
  (format t "~%--- DIAGNOSIS RESULTS ---~%")
    ;; Test multiple car problems
  (let ((problems '((car-problem dead-battery)
                    (car-problem starter-failure)
                    (car-problem fuel-system)
                    (car-problem ignition-system)
                    (car-problem overheating)
                    (car-problem brake-system)
                    (car-problem transmission-slip))))    (let ((found-problems nil))
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
                          (brake-system "Brake System")
                          (transmission-slip "Transmission Slipping")
                          (otherwise "Unknown Problem"))
                        (* (abs cf) 100)))))
          (format t "No specific diagnosis found with current evidence.~%")))))

(defun simple-diagnosis ()
  "Simple interactive diagnosis using backward chaining"
  (format t "~%=== INTERACTIVE DIAGNOSIS ===~%")
  (clear-facts)
  
  (format t "Does your car start? (y/n): ")
  (let ((starts (read)))
    (if (or (eq starts 'y) (eq starts 'yes))
        (add-fact '(car starts) 0.9)
        (add-fact '(car does-not-start) 0.9)))
  
  (when (fact-known-p '(car does-not-start))
    (format t "Do the lights work normally? (y/n): ")
    (let ((lights (read)))
      (if (or (eq lights 'y) (eq lights 'yes))
          (add-fact '(lights work) 0.8)
          (add-fact '(lights dim-or-off) 0.8))))
  
  (when (fact-known-p '(car starts))
    (format t "Are there warning lights on dashboard? (y/n): ")
    (let ((warning (read)))
      (when (or (eq warning 'y) (eq warning 'yes))
        (add-fact '(dashboard warning-lights) 0.8))))
  
  (format t "~%Running backward chaining diagnosis...~%")
  (show-diagnosis-results))

(defun system-status ()
  "Show system status information"
  (format t "~%=== SYSTEM STATUS ===~%")
  (format t "Backward Chaining Car Expert System~%")
  (format t "Package: ~A~%" (package-name *package*))
  (format t "Number of facts: ~A~%" (hash-table-count *facts*))
  (format t "Number of rules: ~A~%" (length *rules*))
  (format t "System ready for diagnosis~%"))

;; Start the demo
(format t "~%Car Expert System Demo loaded!~%")
(format t "Type (run-demo) to start interactive demonstrations~%")
(format t "Type (simple-diagnosis) for quick diagnosis~%")
(format t "Type (system-status) to see system information~%")
