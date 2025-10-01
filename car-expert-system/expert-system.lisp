;; =============================================================================
;; EXPERT-SYSTEM.LISP
;; Core Expert System Engine for Car Troubleshooting
;; =============================================================================

;; Global variables for the expert system
(defvar *facts* nil "Current facts in the knowledge base")
(defvar *rules* nil "Rules in the knowledge base")
(defvar *trace-mode* nil "Enable tracing of inference process")

;; =============================================================================
;; FACT MANAGEMENT
;; =============================================================================

(defun add-fact (fact)
  "Add a fact to the knowledge base"
  (unless (member fact *facts* :test #'equal)
    (push fact *facts*)
    (when *trace-mode*
      (format t "Added fact: ~A~%" fact)))
  fact)

(defun remove-fact (fact)
  "Remove a fact from the knowledge base"
  (setf *facts* (remove fact *facts* :test #'equal))
  (when *trace-mode*
    (format t "Removed fact: ~A~%" fact)))

(defun fact-exists-p (fact)
  "Check if a fact exists in the knowledge base"
  (member fact *facts* :test #'equal))

(defun clear-facts ()
  "Clear all facts from the knowledge base"
  (setf *facts* nil))

(defun show-facts ()
  "Display all current facts"
  (format t "~%Current Facts:~%")
  (if *facts*
      (dolist (fact *facts*)
        (format t "  - ~A~%" fact))
      (format t "  (no facts)~%")))

;; =============================================================================
;; RULE MANAGEMENT
;; =============================================================================

(defstruct rule
  name          ; Rule identifier
  conditions    ; List of conditions (facts that must be true)
  actions       ; List of actions (facts to add when rule fires)
  description   ; Human-readable description
  fired)        ; Flag to track if rule has been used

(defun add-rule (name conditions actions &optional description)
  "Add a rule to the knowledge base"
  (let ((new-rule (make-rule :name name
                            :conditions conditions
                            :actions actions
                            :description description
                            :fired nil)))
    (setf *rules* (remove name *rules* :key #'rule-name :test #'equal))
    (push new-rule *rules*)
    (when *trace-mode*
      (format t "Added rule: ~A~%" name))
    new-rule))

(defun find-rule (name)
  "Find a rule by name"
  (find name *rules* :key #'rule-name :test #'equal))

(defun clear-rules ()
  "Clear all rules and reset fired flags"
  (setf *rules* nil))

(defun reset-rules ()
  "Reset all rule fired flags"
  (dolist (rule *rules*)
    (setf (rule-fired rule) nil)))

(defun show-rules ()
  "Display all rules"
  (format t "~%Rules in Knowledge Base:~%")
  (if *rules*
      (dolist (rule *rules*)
        (format t "  ~A: ~A~%" 
                (rule-name rule)
                (or (rule-description rule) "No description")))
      (format t "  (no rules)~%")))

;; =============================================================================
;; PATTERN MATCHING
;; =============================================================================

(defun pattern-match (pattern fact)
  "Match a pattern against a fact, allowing for variables"
  (cond
    ((and (null pattern) (null fact)) t)
    ((or (null pattern) (null fact)) nil)
    ((eql pattern fact) t)
    ((and (listp pattern) (listp fact))
     (and (pattern-match (first pattern) (first fact))
          (pattern-match (rest pattern) (rest fact))))
    ((and (symbolp pattern) 
          (char= (char (symbol-name pattern) 0) #\?))
     t) ; Variable matches anything
    (t nil)))

(defun conditions-satisfied-p (conditions)
  "Check if all conditions in a list are satisfied"
  (every (lambda (condition)
           (some (lambda (fact)
                   (pattern-match condition fact))
                 *facts*))
         conditions))

;; =============================================================================
;; INFERENCE ENGINE
;; =============================================================================

(defun fire-rule (rule)
  "Execute a rule by adding its actions as facts"
  (when *trace-mode*
    (format t "Firing rule: ~A~%" (rule-name rule)))
  (dolist (action (rule-actions rule))
    (add-fact action))
  (setf (rule-fired rule) t))

(defun find-applicable-rules ()
  "Find all rules whose conditions are satisfied and haven't fired"
  (remove-if (lambda (rule)
               (or (rule-fired rule)
                   (not (conditions-satisfied-p (rule-conditions rule)))))
             *rules*))

(defun forward-chain ()
  "Perform forward chaining inference"
  (let ((iteration 0)
        (max-iterations 50))
    (when *trace-mode*
      (format t "~%Starting forward chaining...~%"))
    
    (loop
      (let ((applicable-rules (find-applicable-rules)))
        (incf iteration)
        
        (when *trace-mode*
          (format t "~%Iteration ~A: Found ~A applicable rule(s)~%" 
                  iteration (length applicable-rules)))
        
        (if (or (null applicable-rules) 
                (> iteration max-iterations))
            (return)
            (dolist (rule applicable-rules)
              (fire-rule rule)))))
    
    (when *trace-mode*
      (format t "Forward chaining completed after ~A iterations.~%" iteration))))

;; =============================================================================
;; USER INTERACTION
;; =============================================================================

(defun ask-question (question &optional (type 'yes-no))
  "Ask the user a question and return the response"
  (format t "~%~A " question)
  (case type
    (yes-no
     (format t "(yes/no): ")
     (let ((response (read-line)))
       (member (string-downcase (string-trim " " response))
               '("yes" "y" "true" "1") :test #'string=)))
    (choice
     (format t "Enter your choice: ")
     (string-trim " " (read-line)))
    (otherwise
     (read-line))))

(defun get-user-input (prompt)
  "Get text input from user"
  (format t "~A: " prompt)
  (string-trim " " (read-line)))

;; =============================================================================
;; DEBUGGING AND UTILITIES
;; =============================================================================

(defun enable-trace ()
  "Enable tracing mode"
  (setf *trace-mode* t)
  (format t "Tracing enabled.~%"))

(defun disable-trace ()
  "Disable tracing mode"
  (setf *trace-mode* nil)
  (format t "Tracing disabled.~%"))

(defun system-status ()
  "Display current system status"
  (format t "~%=== EXPERT SYSTEM STATUS ===~%")
  (format t "Facts: ~A~%" (length *facts*))
  (format t "Rules: ~A~%" (length *rules*))
  (format t "Tracing: ~A~%" (if *trace-mode* "ON" "OFF"))
  (show-facts)
  (show-rules))

(defun reset-system ()
  "Reset the entire expert system"
  (clear-facts)
  (reset-rules)
  (format t "Expert system reset.~%"))

;; =============================================================================
;; CONSULTATION ENGINE
;; =============================================================================

(defun start-consultation ()
  "Start an expert system consultation"
  (format t "~%=== CAR TROUBLESHOOTING EXPERT SYSTEM ===~%")
  (format t "I'll help you diagnose car problems.~%")
  (format t "Please answer the questions honestly.~%")
  
  (reset-system)
  (gather-initial-symptoms)
  (forward-chain)
  (provide-diagnosis))

(defun gather-initial-symptoms ()
  "Collect initial symptoms from the user"
  (format t "~%Let's start with some basic questions:~%")
  
  ;; Car starts
  (when (ask-question "Does your car start?")
    (add-fact '(car starts)))
  
  (unless (fact-exists-p '(car starts))
    (add-fact '(car does-not-start)))
  
  ;; Engine running
  (when (and (fact-exists-p '(car starts))
             (ask-question "Does the engine run smoothly?"))
    (add-fact '(engine runs-smoothly)))
  
  (when (and (fact-exists-p '(car starts))
             (not (fact-exists-p '(engine runs-smoothly)))
             (ask-question "Does the engine make unusual noises?"))
    (add-fact '(engine unusual-noises)))
  
  ;; Dashboard lights
  (when (ask-question "Are there any warning lights on the dashboard?")
    (add-fact '(dashboard warning-lights)))
  
  ;; Recent maintenance
  (when (ask-question "Has the car had recent maintenance or repairs?")
    (add-fact '(recent maintenance))))

(defun provide-diagnosis ()
  "Provide diagnosis based on current facts"
  (format t "~%=== DIAGNOSIS ===~%")
  
  (let ((diagnoses (remove-if-not 
                    (lambda (fact) 
                      (and (listp fact) 
                           (eq (first fact) 'diagnosis)))
                    *facts*)))
    
    (if diagnoses
        (progn
          (format t "Based on the symptoms, possible issues include:~%")
          (dolist (diagnosis diagnoses)
            (format t "  • ~A~%" (rest diagnosis))))
        (format t "I couldn't determine a specific diagnosis.~%"))
    
    (format t "~%Recommended actions:~%")
    (let ((recommendations (remove-if-not 
                           (lambda (fact) 
                             (and (listp fact) 
                                  (eq (first fact) 'recommend)))
                           *facts*)))
      
      (if recommendations
          (dolist (recommendation recommendations)
            (format t "  • ~A~%" (rest recommendation)))
          (format t "  • Consult a qualified mechanic for further diagnosis~%")))))

;; Export main functions
(format t "Expert system engine loaded successfully.~%")