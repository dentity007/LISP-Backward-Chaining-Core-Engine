;;;; Car Diagnostic Rules for Backward Chaining Expert System
;;;; Converted from forward chaining to goal-oriented reasoning with certainty factors

(in-package :expert-system)

;; Register domain contradictions so that contradictory evidence yields
;; negative certainty for rules requiring the opposite condition.
;; Examples: (lights work) vs (lights dim-or-off), (car starts) vs (car does-not-start)
(register-contradiction '(lights work) '(lights dim-or-off))
(register-contradiction '(car starts) '(car does-not-start))
(register-contradiction '(engine running) '(car does-not-start))

;; =============================================================================
;; MAIN DIAGNOSTIC GOALS
;; =============================================================================

;; Top-level car problems that can be diagnosed
(define-rule car-problem-dead-battery
  '(car-problem dead-battery)
  '((car does-not-start) (lights dim-or-off))
  0.9
  "Are the headlights and dashboard lights dim or completely off?")

(define-rule car-problem-starter-failure
  '(car-problem starter-failure)
  '((car does-not-start) (lights work) (clicking-sound))
  0.85
  "Do you hear a clicking sound when you turn the key?")

(define-rule car-problem-fuel-system
  '(car-problem fuel-system)
  '((car does-not-start) (engine turns-over) (no-fuel-smell))
  0.8
  "Can you smell gasoline when trying to start?")

(define-rule car-problem-ignition-system
  '(car-problem ignition-system)
  '((car does-not-start) (engine turns-over) (no-spark))
  0.8
  "Have you checked if there's spark at the plugs?")

(define-rule car-problem-engine-misfire
  '(car-problem engine-misfire)
  '((car starts) (engine rough-idle))
  0.75
  "Does the engine run roughly or shake when idling?")

(define-rule car-problem-overheating
  '(car-problem overheating)
  '((engine running) (temperature high) (coolant low))
  0.95
  "Is the temperature gauge reading high and coolant level low?")

(define-rule car-problem-low-oil-pressure
  '(car-problem low-oil-pressure)
  '((engine running) (oil-pressure-light on))
  0.9
  "Is the oil pressure warning light on?")

(define-rule car-problem-transmission-slip
  '(car-problem transmission-slip)
  '((car starts) (engine revs-high) (acceleration poor))
  0.8
  "Does the engine rev high but the car doesn't accelerate properly?")

(define-rule car-problem-brake-system
  '(car-problem brake-system)
  '((brake-warning-light on))
  0.95
  "Is the brake warning light on?")

(define-rule car-problem-charging-system
  '(car-problem charging-system)
  '((car starts) (battery-light on) (electrical-problems))
  0.85
  "Is the battery warning light on while driving?")

;; =============================================================================
;; SYMPTOM IDENTIFICATION RULES
;; =============================================================================

;; Car starting symptoms
(define-rule symptom-car-does-not-start
  '(car does-not-start)
  '()
  1.0
  "Does your car start when you turn the key?")

(define-rule symptom-car-starts
  '(car starts)
  '()
  1.0
  "Does your car start normally?")

(define-rule symptom-lights-work
  '(lights work)
  '()
  1.0
  "Do the headlights and dashboard lights work normally?")

(define-rule symptom-lights-dim-or-off
  '(lights dim-or-off)
  '()
  1.0
  "Are the headlights and dashboard lights dim or completely off?")

(define-rule symptom-engine-turns-over
  '(engine turns-over)
  '()
  1.0
  "Does the engine turn over (crank) when you try to start it?")

(define-rule symptom-clicking-sound
  '(clicking-sound)
  '()
  1.0
  "Do you hear a clicking sound when you turn the key?")

(define-rule symptom-no-fuel-smell
  '(no-fuel-smell)
  '()
  1.0
  "Have you recently run low on gas or notice no fuel smell?")

(define-rule symptom-no-spark
  '(no-spark)
  '()
  1.0
  "Have you checked if there's spark at the spark plugs?")

;; Engine running symptoms
(define-rule symptom-engine-running
  '(engine running)
  '((car starts))
  0.9)

(define-rule symptom-engine-rough-idle
  '(engine rough-idle)
  '()
  1.0
  "Does the engine run roughly, shake, or idle unevenly?")

(define-rule symptom-temperature-high
  '(temperature high)
  '()
  1.0
  "Is the temperature gauge reading high or in the red zone?")

(define-rule symptom-coolant-low
  '(coolant low)
  '()
  1.0
  "Is the coolant level low in the reservoir?")

(define-rule symptom-oil-pressure-light-on
  '(oil-pressure-light on)
  '()
  1.0
  "Is the oil pressure warning light illuminated?")

(define-rule symptom-engine-revs-high
  '(engine revs-high)
  '()
  1.0
  "Does the engine rev high when you press the accelerator?")

(define-rule symptom-acceleration-poor
  '(acceleration poor)
  '()
  1.0
  "Does the car accelerate poorly despite engine revving?")

;; Brake symptoms
(define-rule symptom-brake-warning-light-on
  '(brake-warning-light on)
  '()
  1.0
  "Is the brake warning light on your dashboard illuminated?")

;; Electrical symptoms
(define-rule symptom-battery-light-on
  '(battery-light on)
  '()
  1.0
  "Is the battery warning light on while driving?")

(define-rule symptom-electrical-problems
  '(electrical-problems)
  '()
  1.0
  "Are other electrical components (radio, lights, etc.) acting strangely?")

;; =============================================================================
;; CONSULTATION FUNCTIONS
;; =============================================================================

;; Define helpers before use to avoid compilation warnings
(defun get-problem-description (problem)
  "Get human-readable description for a problem"
  (case (second problem)
    (dead-battery "Dead Battery")
    (starter-failure "Starter Motor Failure")
    (fuel-system "Fuel System Problem")
    (ignition-system "Ignition System Problem")
    (engine-misfire "Engine Misfire")
    (overheating "Engine Overheating")
    (low-oil-pressure "Low Oil Pressure")
    (transmission-slip "Transmission Slipping")
    (brake-system "Brake System Problem")
    (charging-system "Charging System Problem")
    (t "Unknown Problem")))

(defun print-recommendations (problem)
  "Print recommendations for a specific problem"
  (case (second problem)
    (dead-battery 
     (format t "  - Check battery voltage and connections~%")
     (format t "  - Jump start or replace battery~%")
     (format t "  - Have charging system tested~%"))
    
    (starter-failure
     (format t "  - Have starter motor tested~%")
     (format t "  - Check starter connections~%")
     (format t "  - Test starter solenoid~%"))
    
    (fuel-system
     (format t "  - Check fuel level~%")
     (format t "  - Test fuel pump~%")
     (format t "  - Check fuel filter~%")
     (format t "  - Inspect fuel injectors~%"))
    
    (ignition-system
     (format t "  - Check spark plugs~%")
     (format t "  - Test ignition coils~%")
     (format t "  - Check distributor cap and rotor~%")
     (format t "  - Test ignition timing~%"))
    
    (engine-misfire
     (format t "  - Check spark plugs and wires~%")
     (format t "  - Clean fuel injectors~%")
     (format t "  - Check for vacuum leaks~%")
     (format t "  - Test compression~%"))
    
    (overheating
     (format t "  - STOP DRIVING IMMEDIATELY - ENGINE DAMAGE RISK~%")
     (format t "  - Check for coolant leaks~%")
     (format t "  - Test radiator cap~%")
     (format t "  - Inspect water pump~%"))
    
    (low-oil-pressure
     (format t "  - STOP DRIVING IMMEDIATELY - ENGINE DAMAGE RISK~%")
     (format t "  - Check oil level~%")
     (format t "  - Have oil pump tested~%")
     (format t "  - Check for oil leaks~%"))
    
    (transmission-slip
     (format t "  - Check transmission fluid level~%")
     (format t "  - Inspect for fluid leaks~%")
     (format t "  - Have transmission serviced~%")
     (format t "  - Test transmission bands/clutches~%"))
    
    (brake-system
     (format t "  - DO NOT DRIVE - SAFETY RISK~%")
     (format t "  - Check brake fluid level~%")
     (format t "  - Inspect brake pads~%")
     (format t "  - Have brakes inspected immediately~%"))
    
    (charging-system
     (format t "  - Test alternator output~%")
     (format t "  - Check drive belt~%")
     (format t "  - Inspect battery connections~%")
     (format t "  - Test voltage regulator~%"))))

(defun diagnose-car-problem ()
  "Start a car diagnostic consultation"
  (clear-session)
  (format t "~&=== CAR DIAGNOSTIC EXPERT SYSTEM ===~%")
  (format t "I'll help you diagnose your car problem.~%")
  (format t "Please answer the questions as accurately as possible.~%~%")
  
  ;; Try to diagnose main problems
  (let ((problems '((car-problem dead-battery)
                   (car-problem starter-failure)
                   (car-problem fuel-system)
                   (car-problem ignition-system)
                   (car-problem engine-misfire)
                   (car-problem overheating)
                   (car-problem low-oil-pressure)
                   (car-problem transmission-slip)
                   (car-problem brake-system)
                   (car-problem charging-system)))
        (found-problems nil))
    
    (dolist (problem problems)
      (let ((cf (prove-goal problem)))
        (when (certainty-true-p cf)
          (push (list problem cf) found-problems))))
    
    ;; Display results
    (format t "~&~%=== DIAGNOSTIC RESULTS ===~%")
    (if found-problems
        (progn
          (format t "Likely problems found:~%")
          (dolist (problem-cf found-problems)
            (let ((problem (first problem-cf))
                  (cf (second problem-cf)))
              (format t "  • ~A (confidence: ~,1F%)~%" 
                      (get-problem-description problem) (* cf 100))))
          
          (format t "~%=== RECOMMENDATIONS ===~%")
          (dolist (problem-cf found-problems)
            (let ((problem (first problem-cf)))
              (format t "~%For ~A:~%" (get-problem-description problem))
              (print-recommendations problem))))
        (format t "No specific problems could be identified with sufficient confidence.~%"))
    
    found-problems))

;; =============================================================================
;; DEMO FUNCTION
;; =============================================================================

(defun demo-car-diagnosis ()
  "Quick demonstration of car diagnosis"
  (format t "~&=== CAR DIAGNOSIS DEMO ===~%")
  (format t "This demo will show backward chaining diagnosis.~%~%")
  
  (enable-trace)
  (clear-session)
  
  ;; Simulate a dead battery scenario
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  
  (format t "Simulating: Car won't start, lights are dim~%")
  (let ((cf (prove-goal '(car-problem dead-battery))))
    (format t "~%Result: Dead battery diagnosis with CF: ~,2F~%" cf)))

;; Initialize the car diagnostic system
(format t "~&Car diagnostic rules loaded successfully.~%")
(format t "Use (diagnose-car-problem) to start a consultation.~%")
(format t "Use (demo-car-diagnosis) for a quick demo.~%")
