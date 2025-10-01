;; =============================================================================
;; RUN.LISP
;; Main Runner for Car Troubleshooting Expert System
;; =============================================================================

;; Load the car expert system
(load "car-rules.lisp")

;; =============================================================================
;; MAIN CONSULTATION FUNCTION
;; =============================================================================

(defun run-car-diagnosis ()
  "Main function to run a complete car diagnosis consultation"
  (format t "~%")
  (format t "================================================~%")
  (format t "    CAR TROUBLESHOOTING EXPERT SYSTEM~%")
  (format t "================================================~%")
  (format t "~%Welcome! I'm your virtual car diagnostic assistant.~%")
  (format t "I'll help you diagnose common car problems by asking~%")
  (format t "a series of questions about your vehicle's symptoms.~%")
  (format t "~%Please answer questions with 'yes' or 'no' unless~%")
  (format t "otherwise specified.~%")
  (format t "~%Let's begin...~%")
  
  ;; Reset the system for a fresh consultation
  (reset-system)
  
  ;; Gather initial information
  (gather-basic-information)
  
  ;; Ask detailed follow-up questions
  (ask-detailed-questions)
  
  ;; Run the inference engine
  (format t "~%Analyzing symptoms...~%")
  (forward-chain)
  
  ;; Provide the diagnosis and recommendations
  (provide-final-diagnosis)
  
  ;; Offer additional options
  (consultation-menu))

(defun gather-basic-information ()
  "Collect basic information about the car problem"
  (format t "~%=== BASIC INFORMATION ===~%")
  
  ;; Primary symptom
  (format t "~%What is the main problem with your car?~%")
  (format t "1. Won't start at all~%")
  (format t "2. Starts but runs poorly~%")
  (format t "3. Warning lights are on~%")
  (format t "4. Strange noises~%")
  (format t "5. Other problem~%")
  
  (let ((choice (get-user-input "Enter choice (1-5)")))
    (case (parse-integer choice :junk-allowed t)
      (1 (add-fact '(car does-not-start)))
      (2 (add-fact '(car starts))
         (add-fact '(engine runs-poorly)))
      (3 (add-fact '(car starts))
         (add-fact '(dashboard warning-lights)))
      (4 (add-fact '(car starts))
         (add-fact '(engine unusual-noises)))
      (5 (add-fact '(car starts))
         (add-fact '(other-problem)))))
  
  ;; How long has this been happening?
  (let ((duration (get-user-input "How long has this problem been occurring? (recent/weeks/months)")))
    (cond
      ((string= (string-downcase duration) "recent")
       (add-fact '(problem recent)))
      ((string= (string-downcase duration) "weeks")
       (add-fact '(problem weeks)))
      ((string= (string-downcase duration) "months")
       (add-fact '(problem months)))))
  
  ;; Recent maintenance
  (when (ask-question "Has your car had any recent maintenance or repairs?")
    (add-fact '(recent maintenance))))

(defun provide-final-diagnosis ()
  "Provide comprehensive diagnosis and recommendations"
  (format t "~%")
  (format t "================================================~%")
  (format t "           DIAGNOSIS RESULTS~%")
  (format t "================================================~%")
  
  ;; Show what we determined
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
    
    ;; Display possible causes
    (if diagnoses
        (progn
          (format t "~%POSSIBLE CAUSES:~%")
          (loop for i from 1
                for diagnosis in diagnoses
                do (format t "~A. ~A~%" i (string-capitalize 
                                           (format nil "~{~A~^ ~}" (rest diagnosis))))))
        (format t "~%Could not determine specific cause based on symptoms.~%"))
    
    ;; Display recommendations
    (if recommendations
        (progn
          (format t "~%RECOMMENDED ACTIONS:~%")
          (loop for i from 1
                for recommendation in recommendations
                do (let ((rec-text (if (listp (second recommendation))
                                      (format nil "~{~A~^ ~}" (rest recommendation))
                                      (format nil "~A" (second recommendation)))))
                     (format t "~A. ~A~%" i rec-text))))
        (progn
          (format t "~%GENERAL RECOMMENDATION:~%")
          (format t "1. Consult a qualified mechanic for professional diagnosis~%")))
    
    ;; Safety warnings
    (when (some (lambda (fact)
                  (and (listp fact)
                       (eq (first fact) 'recommend)
                       (search "STOP" (format nil "~A" fact))))
                *facts*)
      (format t "~%*** SAFETY WARNING ***~%")
      (format t "Your car may have a serious safety issue.~%")
      (format t "Do not drive until the problem is resolved!~%"))
    
    ;; Cost estimate (basic)
    (when diagnoses
      (provide-cost-estimate diagnoses)))))

(defun provide-cost-estimate (diagnoses)
  "Provide rough cost estimates for common problems"
  (format t "~%ROUGH COST ESTIMATES:~%")
  (format t "(Prices vary by location and vehicle type)~%")
  
  (dolist (diagnosis diagnoses)
    (let ((problem (rest diagnosis)))
      (cond
        ((member 'battery problem)
         (format t "• Battery replacement: $50-$200~%"))
        ((member 'starter problem)
         (format t "• Starter repair: $200-$500~%"))
        ((member 'brake problem)
         (format t "• Brake repair: $100-$800~%"))
        ((member 'alternator problem)
         (format t "• Alternator replacement: $300-$800~%"))
        ((member 'transmission problem)
         (format t "• Transmission service: $100-$3000+~%"))
        ((member 'engine problem)
         (format t "• Engine diagnosis/repair: $100-$2000+~%"))
        (t (format t "• Professional diagnosis: $100-$150~%"))))))

(defun consultation-menu ()
  "Provide post-diagnosis options"
  (format t "~%")
  (format t "================================================~%")
  (format t "              OPTIONS~%")
  (format t "================================================~%")
  (format t "1. Start new diagnosis~%")
  (format t "2. Show detailed system status~%")
  (format t "3. Enable diagnostic tracing~%")
  (format t "4. Run demo scenarios~%")
  (format t "5. Exit~%")
  
  (let ((choice (get-user-input "~%Choose an option (1-5)")))
    (case (parse-integer choice :junk-allowed t)
      (1 (run-car-diagnosis))
      (2 (system-status)
         (consultation-menu))
      (3 (enable-trace)
         (format t "Tracing enabled. Run new diagnosis to see detailed inference process.~%")
         (consultation-menu))
      (4 (run-demo-scenarios)
         (consultation-menu))
      (5 (format t "Thank you for using the Car Troubleshooting Expert System!~%")
         (format t "Drive safely!~%"))
      (otherwise 
       (format t "Invalid choice. Please try again.~%")
       (consultation-menu)))))

;; =============================================================================
;; DEMO SCENARIOS
;; =============================================================================

(defun run-demo-scenarios ()
  "Demonstrate the expert system with predefined scenarios"
  (format t "~%=== DEMO SCENARIOS ===~%")
  (format t "1. Dead battery scenario~%")
  (format t "2. Overheating engine scenario~%")
  (format t "3. Brake problems scenario~%")
  (format t "4. Transmission issues scenario~%")
  (format t "5. Return to main menu~%")
  
  (let ((choice (get-user-input "Choose demo (1-5)")))
    (case (parse-integer choice :junk-allowed t)
      (1 (demo-dead-battery))
      (2 (demo-overheating))
      (3 (demo-brake-problems))
      (4 (demo-transmission-issues))
      (5 (return-from run-demo-scenarios))
      (otherwise 
       (format t "Invalid choice.~%")
       (run-demo-scenarios)))))

(defun demo-dead-battery ()
  "Demonstrate dead battery diagnosis"
  (format t "~%=== DEAD BATTERY DEMO ===~%")
  (reset-system)
  (enable-trace)
  
  (format t "Simulating: Car won't start, lights are dim...~%")
  (add-fact '(car does-not-start))
  (add-fact '(lights dim-or-off))
  
  (forward-chain)
  (show-facts)
  (disable-trace))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== OVERHEATING DEMO ===~%")
  (reset-system)
  (enable-trace)
  
  (format t "Simulating: Engine running hot, coolant low...~%")
  (add-fact '(engine running))
  (add-fact '(temperature high))
  (add-fact '(coolant low))
  
  (forward-chain)
  (show-facts)
  (disable-trace))

(defun demo-brake-problems ()
  "Demonstrate brake problem diagnosis"
  (format t "~%=== BRAKE PROBLEMS DEMO ===~%")
  (reset-system)
  (enable-trace)
  
  (format t "Simulating: Brake warning light on...~%")
  (add-fact '(brake-warning-light on))
  
  (forward-chain)
  (show-facts)
  (disable-trace))

(defun demo-transmission-issues ()
  "Demonstrate transmission problem diagnosis"
  (format t "~%=== TRANSMISSION ISSUES DEMO ===~%")
  (reset-system)
  (enable-trace)
  
  (format t "Simulating: Car starts, engine revs high, poor acceleration...~%")
  (add-fact '(car starts))
  (add-fact '(engine revs-high))
  (add-fact '(acceleration poor))
  
  (forward-chain)
  (show-facts)
  (disable-trace))

;; =============================================================================
;; MAIN ENTRY POINT
;; =============================================================================

(defun main ()
  "Main entry point for the car expert system"
  (run-car-diagnosis))

;; Start the system
(format t "~%Car Expert System loaded and ready!~%")
(format t "Type (main) to start a consultation~%")
(format t "Type (run-demo-scenarios) to see demonstrations~%")
(format t "Type (system-status) to see system information~%")