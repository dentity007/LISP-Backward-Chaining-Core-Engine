;; =============================================================================
;; CAR-RULES.LISP
;; Knowledge Base for Car Troubleshooting Expert System
;; =============================================================================

;; Load the expert system engine first
(load "expert-system.lisp")

;; =============================================================================
;; CAR STARTING PROBLEMS
;; =============================================================================

(add-rule 'dead-battery-rule
          '((car does-not-start) (lights dim-or-off))
          '((diagnosis dead battery) 
            (recommend "Check battery voltage and connections")
            (recommend "Jump start or replace battery"))
          "Car won't start and lights are dim - likely dead battery")

(add-rule 'starter-problem-rule
          '((car does-not-start) (lights work) (clicking-sound))
          '((diagnosis faulty starter)
            (recommend "Have starter motor tested")
            (recommend "Check starter connections"))
          "Car won't start but lights work with clicking sound")

(add-rule 'fuel-problem-rule
          '((car does-not-start) (engine turns-over) (no fuel-smell))
          '((diagnosis fuel system problem)
            (recommend "Check fuel level")
            (recommend "Test fuel pump")
            (recommend "Check fuel filter"))
          "Engine turns over but won't start - possible fuel issue")

(add-rule 'ignition-problem-rule
          '((car does-not-start) (engine turns-over) (no spark))
          '((diagnosis ignition system problem)
            (recommend "Check spark plugs")
            (recommend "Test ignition coils")
            (recommend "Check distributor cap and rotor"))
          "Engine turns over but no spark")

;; =============================================================================
;; ENGINE PERFORMANCE PROBLEMS
;; =============================================================================

(add-rule 'rough-idle-rule
          '((car starts) (engine rough-idle))
          '((diagnosis engine misfire)
            (recommend "Check spark plugs and wires")
            (recommend "Clean fuel injectors")
            (recommend "Check vacuum leaks"))
          "Engine runs but idles roughly")

(add-rule 'overheating-rule
          '((engine running) (temperature high) (coolant low))
          '((diagnosis cooling system problem)
            (recommend "Check for coolant leaks")
            (recommend "Test radiator cap")
            (recommend "Inspect water pump")
            (recommend "STOP DRIVING - ENGINE DAMAGE RISK"))
          "Engine is overheating")

(add-rule 'oil-pressure-rule
          '((engine running) (oil-pressure-light on))
          '((diagnosis low oil pressure)
            (recommend "Check oil level immediately")
            (recommend "STOP DRIVING - ENGINE DAMAGE RISK")
            (recommend "Have oil pump tested"))
          "Oil pressure warning light is on")

(add-rule 'check-engine-light-rule
          '((dashboard warning-lights) (check-engine-light on))
          '((diagnosis emissions or engine control problem)
            (recommend "Get OBD-II diagnostic scan")
            (recommend "Check gas cap")
            (recommend "Inspect air filter"))
          "Check engine light is illuminated")

;; =============================================================================
;; TRANSMISSION PROBLEMS
;; =============================================================================

(add-rule 'transmission-slip-rule
          '((car starts) (engine revs-high) (acceleration poor))
          '((diagnosis transmission slipping)
            (recommend "Check transmission fluid level")
            (recommend "Inspect for fluid leaks")
            (recommend "Have transmission serviced"))
          "Engine revs but car doesn't accelerate properly")

(add-rule 'hard-shifting-rule
          '((manual-transmission) (difficult shifting))
          '((diagnosis clutch or transmission problem)
            (recommend "Check clutch fluid level")
            (recommend "Test clutch operation")
            (recommend "Check transmission oil"))
          "Manual transmission is hard to shift")

;; =============================================================================
;; BRAKE PROBLEMS
;; =============================================================================

(add-rule 'brake-warning-rule
          '((brake-warning-light on))
          '((diagnosis brake system problem)
            (recommend "Check brake fluid level")
            (recommend "Inspect brake pads")
            (recommend "DO NOT DRIVE - SAFETY RISK")
            (recommend "Have brakes inspected immediately"))
          "Brake warning light indicates serious safety issue")

(add-rule 'squealing-brakes-rule
          '((braking) (squealing-noise))
          '((diagnosis worn brake pads)
            (recommend "Inspect brake pads")
            (recommend "Replace brake pads if thin")
            (recommend "Check brake rotors"))
          "Brakes are squealing when applied")

(add-rule 'soft-brake-pedal-rule
          '((brake-pedal soft) (pedal-goes-to-floor))
          '((diagnosis brake fluid leak or air in system)
            (recommend "DO NOT DRIVE - SAFETY RISK")
            (recommend "Check for brake fluid leaks")
            (recommend "Have brake system bled"))
          "Brake pedal feels soft or goes to floor")

;; =============================================================================
;; ELECTRICAL PROBLEMS
;; =============================================================================

(add-rule 'alternator-problem-rule
          '((car starts) (battery-light on) (electrical-problems))
          '((diagnosis charging system failure)
            (recommend "Test alternator output")
            (recommend "Check drive belt")
            (recommend "Inspect battery connections"))
          "Battery light on while driving indicates charging problem")

(add-rule 'headlight-problem-rule
          '((headlights dim) (dashboard lights-dim))
          '((diagnosis electrical system problem)
            (recommend "Test battery and alternator")
            (recommend "Check all electrical connections")
            (recommend "Inspect wiring harness"))
          "Multiple electrical components are dim or failing")

;; =============================================================================
;; TIRE AND SUSPENSION PROBLEMS
;; =============================================================================

(add-rule 'tire-wear-rule
          '((uneven-tire-wear))
          '((diagnosis alignment or suspension problem)
            (recommend "Check tire pressure")
            (recommend "Have wheel alignment checked")
            (recommend "Inspect suspension components"))
          "Uneven tire wear indicates alignment issues")

(add-rule 'vibration-rule
          '((steering-wheel vibration) (highway-speeds))
          '((diagnosis wheel balance or alignment problem)
            (recommend "Have wheels balanced")
            (recommend "Check tire condition")
            (recommend "Inspect suspension"))
          "Steering wheel vibrates at highway speeds")

;; =============================================================================
;; AIR CONDITIONING PROBLEMS
;; =============================================================================

(add-rule 'ac-not-cooling-rule
          '((ac-on) (air-not-cold))
          '((diagnosis air conditioning problem)
            (recommend "Check refrigerant level")
            (recommend "Inspect AC compressor")
            (recommend "Check AC belt"))
          "Air conditioning is not cooling properly")

;; =============================================================================
;; DIAGNOSTIC HELPER RULES
;; =============================================================================

(add-rule 'gather-more-info-rule
          '((car does-not-start))
          '((need-info lights-work)
            (need-info engine-turns-over)
            (need-info clicking-sound)
            (need-info fuel-smell))
          "Need more information about starting problem")

(add-rule 'engine-performance-info-rule
          '((car starts) (engine unusual-noises))
          '((need-info rough-idle)
            (need-info temperature-gauge)
            (need-info oil-pressure)
            (need-info engine-noise-type))
          "Need more details about engine performance")

;; =============================================================================
;; INTERACTIVE QUESTIONING RULES
;; =============================================================================

(defun ask-detailed-questions ()
  "Ask follow-up questions based on initial symptoms"
  
  ;; If car doesn't start, get more details
  (when (fact-exists-p '(car does-not-start))
    (when (ask-question "Do the headlights and dashboard lights work normally?")
      (add-fact '(lights work)))
    
    (unless (fact-exists-p '(lights work))
      (add-fact '(lights dim-or-off)))
    
    (when (and (fact-exists-p '(lights work))
               (ask-question "Does the engine turn over when you try to start it?"))
      (add-fact '(engine turns-over)))
    
    (when (and (fact-exists-p '(lights work))
               (not (fact-exists-p '(engine turns-over)))
               (ask-question "Do you hear a clicking sound when you turn the key?"))
      (add-fact '(clicking-sound)))
    
    (when (and (fact-exists-p '(engine turns-over))
               (ask-question "Can you smell fuel/gasoline?"))
      (add-fact '(fuel-smell)))
    
    (when (and (fact-exists-p '(engine turns-over))
               (not (fact-exists-p '(fuel-smell)))
               (ask-question "Have you recently run low on gas?"))
      (add-fact '(no fuel-smell))))
  
  ;; If car starts but has problems
  (when (fact-exists-p '(car starts))
    (when (ask-question "Does the engine idle roughly or shake?")
      (add-fact '(engine rough-idle)))
    
    (when (ask-question "Is the temperature gauge reading high or red?")
      (add-fact '(temperature high)))
    
    (when (ask-question "Is the coolant level low?")
      (add-fact '(coolant low)))
    
    (when (ask-question "Is the oil pressure warning light on?")
      (add-fact '(oil-pressure-light on)))
    
    (when (ask-question "Is the check engine light on?")
      (add-fact '(check-engine-light on)))
    
    (when (ask-question "Is the brake warning light on?")
      (add-fact '(brake-warning-light on)))
    
    (when (ask-question "Does the engine rev high but the car accelerate poorly?")
      (add-fact '(engine revs-high))
      (add-fact '(acceleration poor)))
    
    (when (ask-question "Do you have a manual transmission?")
      (add-fact '(manual-transmission)))
    
    (when (and (fact-exists-p '(manual-transmission))
               (ask-question "Is it difficult to shift gears?"))
      (add-fact '(difficult shifting)))
    
    (when (ask-question "Do the brakes squeal when you apply them?")
      (add-fact '(braking))
      (add-fact '(squealing-noise)))
    
    (when (ask-question "Does the brake pedal feel soft or go to the floor?")
      (add-fact '(brake-pedal soft))
      (add-fact '(pedal-goes-to-floor)))
    
    (when (ask-question "Is the battery warning light on while driving?")
      (add-fact '(battery-light on))
      (when (ask-question "Are other electrical components acting strangely?")
        (add-fact '(electrical-problems))))
    
    (when (ask-question "Are the headlights or dashboard lights dimmer than usual?")
      (add-fact '(headlights dim))
      (add-fact '(dashboard lights-dim)))
    
    (when (ask-question "Do you notice uneven tire wear?")
      (add-fact '(uneven-tire-wear)))
    
    (when (ask-question "Does the steering wheel vibrate at highway speeds?")
      (add-fact '(steering-wheel vibration))
      (add-fact '(highway-speeds)))
    
    (when (ask-question "Is the air conditioning on but not cooling?")
      (add-fact '(ac-on))
      (add-fact '(air-not-cold)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defun load-car-knowledge-base ()
  "Initialize the car troubleshooting knowledge base"
  (format t "Car troubleshooting rules loaded successfully.~%")
  (format t "Total rules in knowledge base: ~A~%" (length *rules*))
  (format t "Ready for car diagnosis consultation.~%"))

;; Load the knowledge base
(load-car-knowledge-base)