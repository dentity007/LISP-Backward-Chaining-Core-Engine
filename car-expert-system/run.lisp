;;;; Backward Chaining Car Expert System - Main Interface
;;;; Interactive consultation system with uncertainty handling

;; Load the car expert system
(load "car-rules.lisp")
(in-package :expert-system)

;; =============================================================================
(defun print-problem-recommendations (problem)
  "Print specific recommendations for a car problem"
  (case problem
    ((car-problem dead-battery)
     (format t "  ✓ Test battery voltage (should be 12.6V when off)~%")
     (format t "  ✓ Clean battery terminals and check connections~%")
     (format t "  ✓ Load test battery or check with multimeter~%")
     (format t "  ⚠ Estimated cost: $100-$200 for new battery~%"))
      
    ((car-problem starter-failure)
     (format t "  ✓ Have starter motor tested~%")
     (format t "  ✓ Check starter connections and solenoid~%")
     (format t "  ✓ Test starter relay~%")
     (format t "  ⚠ Estimated cost: $200-$500~%"))
      
    ((car-problem fuel-system)
     (format t "  ✓ Check fuel level and fuel pump~%")
     (format t "  ✓ Test fuel pressure and filter~%")
     (format t "  ✓ Inspect fuel injectors~%")
     (format t "  ⚠ Estimated cost: $100-$600~%"))
      
    ((car-problem ignition-system)
     (format t "  ✓ Check spark plugs and ignition coils~%")
     (format t "  ✓ Test ignition timing and wires~%")
     (format t "  ✓ Inspect distributor components~%")
     (format t "  ⚠ Estimated cost: $150-$800~%"))
      
    ((car-problem engine-misfire)
     (format t "  ✓ Check spark plugs, wires, and coils~%")
     (format t "  ✓ Clean fuel injectors~%")
     (format t "  ✓ Check for vacuum leaks~%")
     (format t "  ⚠ Estimated cost: $100-$1000~%"))
      
    ((car-problem overheating)
     (format t "  🚨 STOP DRIVING IMMEDIATELY - ENGINE DAMAGE RISK~%")
     (format t "  ✓ Check coolant level and leaks~%")
     (format t "  ✓ Test radiator cap and water pump~%")
     (format t "  ⚠ Estimated cost: $200-$1500~%"))
      
    ((car-problem brake-problems)
     (format t "  ⚠ SAFETY CRITICAL - Address immediately~%")
     (format t "  ✓ Check brake fluid level and color~%")
     (format t "  ✓ Inspect brake pads and rotors~%")
     (format t "  ✓ Test brake system for leaks~%")
     (format t "  ⚠ Estimated cost: $200-$800~%"))
      
    (otherwise
     (format t "  ✓ Consult a qualified automotive technician~%")
     (format t "  ✓ Have a comprehensive diagnostic performed~%"))))

(defun consultation-menu ()
  "Display consultation menu and handle user choices"
  (format t "~%=== CAR DIAGNOSTIC CONSULTATION ===~%")
  (format t "1. Full diagnostic consultation~%")
  (format t "2. Quick symptom check~%")
  (format t "3. Run demonstration scenarios~%")
  (format t "4. Exit~%")
  (format t "~%Choose an option (1-4): ")
  (let ((choice (read)))
    (case choice
      (1 (run-comprehensive-diagnosis))
      (2 (quick-demo))
      (3 (run-demo-scenarios))
      (4 (format t "Goodbye!~%"))
      (otherwise 
       (format t "Invalid choice. Please select 1-4.~%")
       (consultation-menu)))))

(defun run-comprehensive-diagnosis ()
  "Run a comprehensive diagnostic consultation"
  (format t "~%=== COMPREHENSIVE CAR DIAGNOSIS ===~%")
  (clear-facts)
  
  ;; Test all major car problems
  (let ((problems '((car-problem dead-battery)
                    (car-problem starter-failure)
                    (car-problem fuel-system)
                    (car-problem ignition-system)
                    (car-problem engine-misfire)
                    (car-problem overheating)
                    (car-problem brake-problems))))
    
    (format t "~%Testing all diagnostic categories...~%")
    
    (let ((diagnosed-problems '()))
      (dolist (problem problems)
        (let ((cf (prove-goal problem)))
          (when (> (abs cf) 0.1)  ; Only include problems with some evidence
            (push (list problem 
                       (case (first problem)
                         (car-problem (case (second problem)
                                       (dead-battery "Dead Battery")
                                       (starter-failure "Starter Failure")
                                       (fuel-system "Fuel System")
                                       (ignition-system "Ignition System")
                                       (engine-misfire "Engine Misfire")
                                       (overheating "Overheating")
                                       (brake-problems "Brake Problems")
                                       (otherwise "Unknown Problem"))))
                       cf)
                  diagnosed-problems))))
      
      (display-diagnosis-results diagnosed-problems))))

;; Main entry points
;; =============================================================================

(defun run-car-diagnosis ()
  "Main function to run a backward chaining car diagnosis consultation"
  (format t "~%")
  (format t "============================================================~%")
  (format t "    BACKWARD CHAINING CAR DIAGNOSTIC EXPERT SYSTEM~%")
  (format t "============================================================~%")
  (format t "~%Welcome! I'm your AI car diagnostic assistant.~%")
  (format t "I use backward chaining inference with certainty factors~%")
  (format t "to help diagnose your car problems.~%")
  (format t "~%I'll ask targeted questions to prove or disprove possible~%")
  (format t "causes, and provide confidence levels for my conclusions.~%")
  (format t "~%Answer with:~%")
  (format t "  • 'y' or 'yes' for definitely yes (0.8 certainty)~%")
  (format t "  • 'n' or 'no' for definitely no (-0.8 certainty)~%")
  (format t "  • Numbers from -1 to 1 for custom certainty~%")
  (format t "  • 'u' or 'unknown' if you're not sure~%")
  (format t "~%Let's begin the diagnosis...~%")
  
  ;; Clear any previous consultation
  (clear-session)
  
  ;; Run comprehensive diagnosis
  (run-comprehensive-diagnosis)
  
  ;; Offer additional options
  (consultation-menu))

(defun run-comprehensive-diagnosis ()
  "Run a comprehensive diagnosis checking all major car problems"
  (format t "~%=== RUNNING DIAGNOSTIC ANALYSIS ===~%")
  (format t "I'll systematically check for common car problems...~%~%")
  
  ;; Define all possible car problems to check
  (let ((car-problems '((car-problem dead-battery "Dead Battery")
                       (car-problem starter-failure "Starter Motor Failure")
                       (car-problem fuel-system "Fuel System Problem")
                       (car-problem ignition-system "Ignition System Problem")
                       (car-problem engine-misfire "Engine Misfire")
                       (car-problem overheating "Engine Overheating")
                       (car-problem low-oil-pressure "Low Oil Pressure")
                       (car-problem transmission-slip "Transmission Slipping")
                       (car-problem brake-system "Brake System Problem")
                       (car-problem charging-system "Charging System Problem")))
        (diagnosed-problems nil))
    
    ;; Try to prove each problem
    (dolist (problem-info car-problems)
      (let* ((problem-goal (first problem-info))
             (problem-name (third problem-info))
             (cf (prove-goal problem-goal)))
        
        (when (> (abs cf) 0.1) ; Only include if we have some evidence
          (push (list problem-goal problem-name cf) diagnosed-problems))))
    
    ;; Display results
    (display-diagnosis-results diagnosed-problems)))

(defun display-diagnosis-results (problems)
  "Display the diagnosis results with confidence levels"
  (format t "~%~%============================================================~%")
  (format t "                    DIAGNOSIS RESULTS~%")
  (format t "============================================================~%")
  
  (if problems
      (progn
        ;; Sort by confidence level (highest first)
        (setf problems (sort problems (lambda (a b) (> (third a) (third b)))))
        
        (format t "~%DIAGNOSTIC FINDINGS:~%")
        (format t "--------------------~%")
        
        (dolist (problem problems)
          (let* ((problem-goal (first problem))
                 (problem-name (second problem))
                 (cf (third problem))
                 (confidence (* (abs cf) 100))
                 (conclusion (cond 
                              ((certainty-true-p cf) "LIKELY")
                              ((certainty-false-p cf) "UNLIKELY") 
                              (t "UNCERTAIN"))))
            
            (format t "~%• ~A: ~A (~,1F% confidence)~%" 
                    problem-name conclusion confidence)
            
            (when (certainty-true-p cf)
              (format t "  → This problem appears to be present~%"))
            (when (certainty-false-p cf)
              (format t "  → This problem appears to be ruled out~%"))))
        
        ;; Show recommendations for likely problems
        (let ((likely-problems (remove-if-not (lambda (p) (certainty-true-p (third p))) problems)))
          (when likely-problems
            (format t "~%~%RECOMMENDATIONS:~%")
            (format t "----------------~%")
            (dolist (problem likely-problems)
              (let ((problem-name (second problem)))
                (format t "~%For ~A:~%" problem-name)
                (print-problem-recommendations (first problem)))))))
      
      (progn
        (format t "~%No specific problems could be identified.~%")
        (format t "Consider consulting a professional mechanic for~%")
        (format t "a comprehensive inspection.~%")))
  
  ;; Show session summary
  (format t "~%~%SESSION SUMMARY:~%")
  (format t "---------------~%")
  (format t "Questions asked: ~A~%" (hash-table-count *asked-questions*))
  (format t "Facts established: ~A~%" (hash-table-count *facts*))
  (format t "Certainty threshold: ~,2F~%" *certainty-threshold*))

(defun print-problem-recommendations (problem-goal)
  "Print specific recommendations for a problem"
  (let ((problem-type (second problem-goal)))
    (case problem-type
      (dead-battery 
       (format t "  ✓ Check battery voltage and connections~%")
       (format t "  ✓ Jump start or replace battery~%")
       (format t "  ✓ Have charging system tested~%")
       (format t "  ⚠ Estimated cost: $50-$200~%"))
      
      (starter-failure
       (format t "  ✓ Have starter motor tested~%")
       (format t "  ✓ Check starter connections and solenoid~%")
       (format t "  ✓ Test starter relay~%")
       (format t "  ⚠ Estimated cost: $200-$500~%"))
      
      (fuel-system
       (format t "  ✓ Check fuel level and fuel pump~%")
       (format t "  ✓ Test fuel pressure and filter~%")
       (format t "  ✓ Inspect fuel injectors~%")
       (format t "  ⚠ Estimated cost: $100-$600~%"))
      
      (ignition-system
       (format t "  ✓ Check spark plugs and ignition coils~%")
       (format t "  ✓ Test ignition timing and wires~%")
       (format t "  ✓ Inspect distributor components~%")
       (format t "  ⚠ Estimated cost: $150-$800~%"))
      
      (engine-misfire
       (format t "  ✓ Check spark plugs, wires, and coils~%")
       (format t "  ✓ Clean fuel injectors~%")
       (format t "  ✓ Check for vacuum leaks~%")
       (format t "  ⚠ Estimated cost: $100-$1000~%"))
      
      (overheating
       (format t "  🚨 STOP DRIVING IMMEDIATELY - ENGINE DAMAGE RISK~%")
       (format t "  ✓ Check coolant level and leaks~%")
       (format t "  ✓ Test radiator cap and water pump~%")
       (format t "  ⚠ Estimated cost: $200-$1500~%"))
      
      (low-oil-pressure
       (format t "  🚨 STOP DRIVING IMMEDIATELY - ENGINE DAMAGE RISK~%")
       (format t "  ✓ Check oil level immediately~%")
       (format t "  ✓ Have oil pump and filter tested~%")
       (format t "  ⚠ Estimated cost: $100-$2000+~%"))
      
      (transmission-slip
       (format t "  ✓ Check transmission fluid level~%")
       (format t "  ✓ Inspect for leaks and service transmission~%")
       (format t "  ✓ Test transmission bands/clutches~%")
       (format t "  ⚠ Estimated cost: $150-$3000+~%"))
      
      (brake-system
       (format t "  🚨 DO NOT DRIVE - SERIOUS SAFETY RISK~%")
       (format t "  ✓ Check brake fluid and pads immediately~%")
       (format t "  ✓ Have entire brake system inspected~%")
       (format t "  ⚠ Estimated cost: $100-$800~%"))
      
      (charging-system
       (format t "  ✓ Test alternator output and belt~%")
       (format t "  ✓ Check battery and all connections~%")
       (format t "  ✓ Test voltage regulator~%")
       (format t "  ⚠ Estimated cost: $200-$800~%")))))

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

(defun show-detailed-facts ()
  "Show all facts with their certainty factors"
  (format t "~%=== DETAILED FACTS AND CERTAINTIES ===~%")
  (if (> (hash-table-count *facts*) 0)
      (progn
        (format t "~%Current facts in the knowledge base:~%")
        (maphash (lambda (fact cf)
                   (let ((confidence (* (abs cf) 100))
                         (polarity (if (>= cf 0) "+" "-")))
                     (format t "  ~A~,2F ~A (~,1F% confidence)~%" 
                             polarity cf fact confidence)))
                 *facts*)
        (format t "~%Legend: + = positive evidence, - = negative evidence~%"))
      (format t "No facts have been established yet.~%")))

(defun toggle-tracing ()
  "Toggle diagnostic tracing on/off"
  (if *trace-enabled*
      (progn
        (disable-trace)
        (format t "Diagnostic tracing is now OFF~%"))
      (progn
        (enable-trace)
        (format t "Diagnostic tracing is now ON~%")
        (format t "You'll see detailed inference steps in the next diagnosis.~%"))))

(defun adjust-certainty-threshold ()
  "Allow user to adjust the certainty threshold"
  (format t "~%Current certainty threshold: ~,2F~%" *certainty-threshold*)
  (format t "Facts need this level of certainty to be considered 'true'.~%")
  (format t "Enter new threshold (0.1 to 0.9, or press Enter to keep current): ")
  (force-output)
  
  (let ((input (string-trim " " (read-line))))
    (unless (string= input "")
      (let ((new-threshold (ignore-errors (read-from-string input))))
        (if (and new-threshold (numberp new-threshold) 
                 (>= new-threshold 0.1) (<= new-threshold 0.9))
            (progn
              (setf *certainty-threshold* new-threshold)
              (format t "Certainty threshold updated to ~,2F~%" *certainty-threshold*))
            (format t "Invalid threshold. Must be between 0.1 and 0.9~%"))))))

(defun show-system-info ()
  "Show system information and statistics"
  (format t "~%=== SYSTEM INFORMATION ===~%")
  (format t "Expert System Type: Backward Chaining with Certainty Factors~%")
  (format t "Inference Method: MYCIN-style goal-driven reasoning~%")
  (format t "Rules loaded: ~A~%" (length *rules*))
  (format t "Current facts: ~A~%" (hash-table-count *facts*))
  (format t "Questions asked: ~A~%" (hash-table-count *asked-questions*))
  (format t "Certainty threshold: ~,2F~%" *certainty-threshold*)
  (format t "Tracing enabled: ~A~%" (if *trace-enabled* "YES" "NO"))
  (format t "~%Available car problems to diagnose:~%")
  (let ((problems '("Dead Battery" "Starter Failure" "Fuel System" "Ignition System"
                   "Engine Misfire" "Overheating" "Low Oil Pressure" 
                   "Transmission Slip" "Brake System" "Charging System")))
    (dolist (problem problems)
      (format t "  • ~A~%" problem))))

;; =============================================================================
;; DEMONSTRATION SCENARIOS
;; =============================================================================

(defun run-demo-scenarios ()
  "Demonstrate the expert system with predefined scenarios"
  (format t "~%=== DEMONSTRATION SCENARIOS ===~%")
  (format t "1. Dead battery scenario~%")
  (format t "2. Engine overheating scenario~%")
  (format t "3. Brake system problem scenario~%")
  (format t "4. Mixed symptoms scenario~%")
  (format t "5. Return to main menu~%")
  
  (format t "~%Choose demo (1-5): ")
  (force-output)
  (let ((choice (string-trim " " (read-line))))
    (case (ignore-errors (parse-integer choice))
      (1 (demo-dead-battery))
      (2 (demo-overheating))
      (3 (demo-brake-problems))
      (4 (demo-mixed-symptoms))
      (5 (return-from run-demo-scenarios))
      (otherwise 
       (format t "Invalid choice.~%")
       (run-demo-scenarios)))))

(defun demo-dead-battery ()
  "Demonstrate dead battery diagnosis"
  (format t "~%=== DEAD BATTERY DEMONSTRATION ===~%")
  (format t "Simulating: Car won't start, lights are dim...~%")
  
  (clear-session)
  (enable-trace)
  
  ;; Set up the scenario
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  
  ;; Prove the goal
  (let ((cf (prove-goal '(car-problem dead-battery))))
    (format t "~%DEMO RESULT: Dead battery diagnosis with certainty: ~,2F~%" cf))
  
  (disable-trace)
  (format t "~%Press Enter to continue...")
  (read-line))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== ENGINE OVERHEATING DEMONSTRATION ===~%")
  (format t "Simulating: Car starts, temperature high, coolant low...~%")
  
  (clear-session)
  (enable-trace)
  
  ;; Set up the scenario
  (add-fact '(car starts) 0.9)
  (add-fact '(temperature high) 0.95)
  (add-fact '(coolant low) 0.8)
  
  ;; Prove the goal
  (let ((cf (prove-goal '(car-problem overheating))))
    (format t "~%DEMO RESULT: Overheating diagnosis with certainty: ~,2F~%" cf))
  
  (disable-trace)
  (format t "~%Press Enter to continue...")
  (read-line))

(defun demo-brake-problems ()
  "Demonstrate brake problem diagnosis"
  (format t "~%=== BRAKE SYSTEM DEMONSTRATION ===~%")
  (format t "Simulating: Brake warning light on...~%")
  
  (clear-session)
  (enable-trace)
  
  ;; Set up the scenario
  (add-fact '(brake-warning-light on) 0.95)
  
  ;; Prove the goal
  (let ((cf (prove-goal '(car-problem brake-system))))
    (format t "~%DEMO RESULT: Brake system problem with certainty: ~,2F~%" cf))
  
  (disable-trace)
  (format t "~%Press Enter to continue...")
  (read-line))

(defun demo-mixed-symptoms ()
  "Demonstrate diagnosis with mixed/uncertain symptoms"
  (format t "~%=== MIXED SYMPTOMS DEMONSTRATION ===~%")
  (format t "Simulating: Car starts but multiple issues...~%")
  
  (clear-session)
  (enable-trace)
  
  ;; Set up a complex scenario with multiple symptoms
  (add-fact '(car starts) 0.8)
  (add-fact '(engine rough-idle) 0.6)
  (add-fact '(battery-light on) 0.7)
  (add-fact '(electrical-problems) 0.5)
  
  ;; Check multiple problems
  (format t "~%Checking for engine misfire...~%")
  (let ((cf1 (prove-goal '(car-problem engine-misfire))))
    (format t "Engine misfire certainty: ~,2F~%" cf1))
  
  (format t "~%Checking for charging system problems...~%")
  (let ((cf2 (prove-goal '(car-problem charging-system))))
    (format t "Charging system problem certainty: ~,2F~%" cf2))
  
  (disable-trace)
  (format t "~%Press Enter to continue...")
  (read-line))

;; =============================================================================
;; MAIN ENTRY POINTS
;; =============================================================================

(defun main ()
  "Main entry point for the backward chaining car expert system"
  (run-car-diagnosis))

(defun quick-demo ()
  "Quick demonstration without full interface"
  (format t "~%=== QUICK DEMO: Testing Dead Battery Scenario ===~%")
  (clear-facts)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (format t "Facts: Car won't start, lights are dim~%")
  (let ((cf (prove-goal '(car-problem dead-battery))))
    (format t "Dead battery diagnosis: CF = ~,2F~%" cf)
    (if (certainty-true-p cf)
        (format t "RESULT: Dead battery is LIKELY (~,1F% confidence)~%" (* cf 100))
        (format t "RESULT: Dead battery is UNLIKELY~%"))))

(defun demo-dead-battery ()
  "Demonstrate dead battery diagnosis"
  (format t "~%=== DEMO: Dead Battery Scenario ===~%")
  (clear-facts)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (add-fact '(engine-cranks-slowly) 0.7)
  (format t "Simulating: Car won't start, lights dim, slow cranking~%")
  (let ((cf (prove-goal '(car-problem dead-battery))))
    (format t "Result: Dead battery CF = ~,2F~%" cf)))

(defun demo-brake-problems ()
  "Demonstrate brake problems diagnosis"
  (format t "~%=== DEMO: Brake Problems Scenario ===~%")
  (clear-facts)
  (add-fact '(brakes squealing) 0.8)
  (add-fact '(brake-pedal soft) 0.7)
  (format t "Simulating: Squealing brakes, soft pedal~%")
  (let ((cf (prove-goal '(car-problem brake-problems))))
    (format t "Result: Brake problems CF = ~,2F~%" cf)))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== DEMO: Overheating Scenario ===~%")
  (clear-facts)
  (add-fact '(temperature-gauge high) 0.9)
  (add-fact '(steam-from-engine) 0.8)
  (format t "Simulating: High temperature, steam from engine~%")
  (let ((cf (prove-goal '(car-problem overheating))))
    (format t "Result: Overheating CF = ~,2F~%" cf)))

(defun demo-mixed-symptoms ()
  "Demonstrate mixed symptoms scenario"
  (format t "~%=== DEMO: Mixed Symptoms Scenario ===~%")
  (clear-facts)
  (add-fact '(engine-rough-idle) 0.7)
  (add-fact '(engine-hesitation) 0.6)
  (add-fact '(poor-acceleration) 0.8)
  (format t "Simulating: Rough idle, hesitation, poor acceleration~%")
  (let ((cf (prove-goal '(car-problem engine-misfire))))
    (format t "Result: Engine misfire CF = ~,2F~%" cf)))

(defun run-demo-scenarios ()
  "Run all demonstration scenarios"
  (format t "~%=== RUNNING ALL DEMO SCENARIOS ===~%")
  (demo-dead-battery)
  (demo-brake-problems)
  (demo-overheating)
  (demo-mixed-symptoms)
  (format t "~%=== ALL DEMOS COMPLETE ===~%"))

;; Initialize the system
(format t "~%============================================================~%")
(format t " Backward Chaining Car Expert System Ready!~%")
(format t "============================================================~%")
(format t "~%Commands:~%")
(format t "  (main) - Start interactive diagnosis~%")
(format t "  (quick-demo) - Run quick demonstration~%")
(format t "  (run-demo-scenarios) - See all demonstrations~%")
(format t "  (diagnose-car-problem) - Direct diagnosis function~%")
(format t "~%System Features:~%")
(format t "  • Goal-driven backward chaining inference~%")
(format t "  • MYCIN-style certainty factors~%")
(format t "  • Interactive question-asking~%")
(format t "  • Confidence levels for all conclusions~%")
(format t "~%Ready for consultation!~%")