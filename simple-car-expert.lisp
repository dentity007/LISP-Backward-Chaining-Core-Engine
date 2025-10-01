;; =============================================================================
;; SIMPLE-CAR-EXPERT.LISP
;; All-in-one car expert system - no external dependencies
;; =============================================================================

;; Global variables
(defvar *facts* nil "Current facts in the knowledge base")
(defvar *rules* nil "Rules in the knowledge base")

;; =============================================================================
;; BASIC FACT MANAGEMENT
;; =============================================================================

(defun add-fact (fact)
  "Add a fact to the knowledge base"
  (unless (member fact *facts* :test #'equal)
    (push fact *facts*))
  fact)

(defun fact-exists-p (fact)
  "Check if a fact exists in the knowledge base"
  (member fact *facts* :test #'equal))

(defun reset-system ()
  "Reset the entire expert system"
  (setf *facts* nil)
  (format t "System reset.~%"))

(defun show-facts ()
  "Display all current facts"
  (format t "~%Current Facts:~%")
  (if *facts*
      (dolist (fact *facts*)
        (format t "  - ~A~%" fact))
      (format t "  (no facts)~%")))

;; =============================================================================
;; SIMPLE RULE SYSTEM
;; =============================================================================

(defstruct rule name conditions actions)

(defun add-simple-rule (name conditions actions)
  "Add a simple rule"
  (push (make-rule :name name :conditions conditions :actions actions) *rules*))

(defun apply-rules ()
  "Apply all applicable rules"
  (let ((applied-any nil))
    (dolist (rule *rules*)
      (when (every (lambda (condition) (fact-exists-p condition)) 
                   (rule-conditions rule))
        (dolist (action (rule-actions rule))
          (unless (fact-exists-p action)
            (add-fact action)
            (setf applied-any t)
            (format t "Applied rule ~A: Added ~A~%" (rule-name rule) action)))))
    (when applied-any
      (apply-rules)))) ; Keep applying until no new facts

;; =============================================================================
;; CAR DIAGNOSTIC RULES
;; =============================================================================

(defun load-car-rules ()
  "Load all car diagnostic rules"
  (setf *rules* nil) ; Clear existing rules
  
  ;; Dead battery rule
  (add-simple-rule 'dead-battery
    '((car does-not-start) (lights dim-or-off))
    '((diagnosis dead-battery) (recommend "Check battery connections") (recommend "Jump start car")))
  
  ;; Starter problem rule  
  (add-simple-rule 'starter-problem
    '((car does-not-start) (lights work) (clicking-sound))
    '((diagnosis faulty-starter) (recommend "Test starter motor") (recommend "Check starter connections")))
  
  ;; Fuel problem rule
  (add-simple-rule 'fuel-problem
    '((car does-not-start) (engine turns-over) (no-fuel-smell))
    '((diagnosis fuel-system-problem) (recommend "Check fuel level") (recommend "Test fuel pump")))
  
  ;; Overheating rule
  (add-simple-rule 'overheating
    '((engine running) (temperature high) (coolant low))
    '((diagnosis cooling-system-problem) (recommend "STOP DRIVING IMMEDIATELY") (recommend "Check for coolant leaks")))
  
  ;; Brake warning rule
  (add-simple-rule 'brake-warning
    '((brake-warning-light on))
    '((diagnosis brake-system-problem) (recommend "DO NOT DRIVE") (recommend "Check brake fluid") (recommend "Have brakes inspected")))
  
  ;; Rough idle rule
  (add-simple-rule 'rough-idle
    '((car starts) (engine rough-idle))
    '((diagnosis engine-misfire) (recommend "Check spark plugs") (recommend "Clean fuel injectors")))
  
  ;; Transmission slip rule
  (add-simple-rule 'transmission-slip
    '((car starts) (engine revs-high) (acceleration poor))
    '((diagnosis transmission-slipping) (recommend "Check transmission fluid") (recommend "Have transmission serviced")))
  
  (format t "Loaded ~A diagnostic rules.~%" (length *rules*)))

;; =============================================================================
;; RESULTS DISPLAY FUNCTION (must come before demo functions)
;; =============================================================================

(defun show-results ()
  "Show diagnosis and recommendations"
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
    
    (format t "~%=== RESULTS ===~%")
    (if diagnoses
        (progn
          (format t "Diagnosis:~%")
          (dolist (diagnosis diagnoses)
            (format t "  • ~A~%" (second diagnosis))))
        (format t "No diagnosis found.~%"))
    
    (if recommendations
        (progn
          (format t "~%Recommendations:~%")
          (dolist (recommendation recommendations)
            (format t "  • ~A~%" (second recommendation))))
        (format t "~%No recommendations.~%"))))

;; =============================================================================
;; DEMO FUNCTIONS
;; =============================================================================

(defun demo-dead-battery ()
  "Demonstrate dead battery diagnosis"
  (format t "~%=== DEAD BATTERY DEMO ===~%")
  (reset-system)
  (add-fact '(car does-not-start))
  (add-fact '(lights dim-or-off))
  (apply-rules)
  (show-results))

(defun demo-overheating ()
  "Demonstrate overheating diagnosis"
  (format t "~%=== OVERHEATING DEMO ===~%")
  (reset-system)
  (add-fact '(engine running))
  (add-fact '(temperature high))
  (add-fact '(coolant low))
  (apply-rules)
  (show-results))

(defun demo-brake-problem ()
  "Demonstrate brake problem diagnosis"
  (format t "~%=== BRAKE PROBLEM DEMO ===~%")
  (reset-system)
  (add-fact '(brake-warning-light on))
  (apply-rules)
  (show-results))

(defun interactive-diagnosis ()
  "Simple interactive diagnosis"
  (format t "~%=== INTERACTIVE CAR DIAGNOSIS ===~%")
  (reset-system)
  
  (format t "Answer Y or N to each question:~%~%")
  
  ;; Does car start?
  (format t "Does your car start? ")
  (finish-output)
  (let ((answer (string-upcase (read-line))))
    (if (member answer '("Y" "YES") :test #'string=)
        (add-fact '(car starts))
        (add-fact '(car does-not-start))))
  
  ;; If doesn't start, check lights
  (when (fact-exists-p '(car does-not-start))
    (format t "Do the headlights work normally? ")
    (finish-output)
    (let ((answer (string-upcase (read-line))))
      (if (member answer '("Y" "YES") :test #'string=)
          (add-fact '(lights work))
          (add-fact '(lights dim-or-off)))))
  
  ;; If car starts, check for warning lights
  (when (fact-exists-p '(car starts))
    (format t "Is the brake warning light on? ")
    (finish-output)
    (let ((answer (string-upcase (read-line))))
      (when (member answer '("Y" "YES") :test #'string=)
        (add-fact '(brake-warning-light on))))
    
    (format t "Does the engine run roughly? ")
    (finish-output)
    (let ((answer (string-upcase (read-line))))
      (when (member answer '("Y" "YES") :test #'string=)
        (add-fact '(engine rough-idle)))))
  
  (format t "~%Analyzing symptoms...~%")
  (apply-rules)
  (show-results))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(format t "~%")
(format t "================================================~%")
(format t "       SIMPLE CAR EXPERT SYSTEM~%")
(format t "================================================~%")

(load-car-rules)

(format t "~%Available commands:~%")
(format t "  (demo-dead-battery)     - Dead battery scenario~%")
(format t "  (demo-overheating)      - Engine overheating scenario~%") 
(format t "  (demo-brake-problem)    - Brake warning scenario~%")
(format t "  (interactive-diagnosis) - Answer questions for diagnosis~%")
(format t "  (show-facts)            - Show current facts~%")
(format t "  (reset-system)          - Clear all facts~%")
(format t "~%System ready! Try a demo command.~%")