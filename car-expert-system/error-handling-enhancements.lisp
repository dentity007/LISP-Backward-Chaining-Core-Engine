;; =============================================================================
;; ERROR-HANDLING-ENHANCEMENTS.LISP
;; Sophisticated error handling for the backward chaining expert system
;; Adds validation, error recovery, and robust input handling
;; =============================================================================

(load "expert-system.lisp")
(in-package :expert-system)

;; =============================================================================
;; CUSTOM ERROR CONDITIONS
;; =============================================================================

(define-condition certainty-factor-error (error)
  ((value :initarg :value :reader certainty-factor-error-value)
   (context :initarg :context :reader certainty-factor-error-context))
  (:report (lambda (condition stream)
             (format stream "Invalid certainty factor ~A in ~A. Must be between -1.0 and 1.0"
                     (certainty-factor-error-value condition)
                     (certainty-factor-error-context condition)))))

(define-condition malformed-rule-error (error)
  ((rule :initarg :rule :reader malformed-rule-error-rule)
   (reason :initarg :reason :reader malformed-rule-error-reason))
  (:report (lambda (condition stream)
             (format stream "Malformed rule: ~A. ~A"
                     (malformed-rule-error-rule condition)
                     (malformed-rule-error-reason condition)))))

(define-condition invalid-fact-error (error)
  ((fact :initarg :fact :reader invalid-fact-error-fact)
   (reason :initarg :reason :reader invalid-fact-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid fact: ~A. ~A"
                     (invalid-fact-error-fact condition)
                     (invalid-fact-error-reason condition)))))

(define-condition system-state-error (error)
  ((operation :initarg :operation :reader system-state-error-operation)
   (reason :initarg :reason :reader system-state-error-reason))
  (:report (lambda (condition stream)
             (format stream "System state error during ~A: ~A"
                     (system-state-error-operation condition)
                     (system-state-error-reason condition)))))

;; =============================================================================
;; VALIDATION FUNCTIONS
;; =============================================================================

(defun validate-certainty-factor (cf context)
  "Validate that a certainty factor is within valid range"
  (unless (and (numberp cf) (>= cf -1.0) (<= cf 1.0))
    (error 'certainty-factor-error
           :value cf
           :context context))
  cf)

(defun validate-rule (rule)
  "Validate rule structure and components"
  (unless (and (listp rule) (>= (length rule) 3))
    (error 'malformed-rule-error
           :rule rule
           :reason "Rule must be a list with at least conclusion, conditions, and certainty factor"))

  (destructuring-bind (conclusion conditions cf &rest rest) rule
    ;; Validate conclusion
    (unless (and (listp conclusion) (symbolp (first conclusion)))
      (error 'malformed-rule-error
             :rule rule
             :reason "Conclusion must be a list starting with a symbol"))

    ;; Validate conditions
    (unless (listp conditions)
      (error 'malformed-rule-error
             :rule rule
             :reason "Conditions must be a list"))

    (dolist (condition conditions)
      (unless (listp condition)
        (error 'malformed-rule-error
               :rule rule
               :reason (format nil "Condition '~A' must be a list" condition))))

    ;; Validate certainty factor
    (validate-certainty-factor cf "rule definition")

    ;; Validate optional components
    (when rest
      (unless (stringp (first rest))
        (error 'malformed-rule-error
               :rule rule
               :reason "Optional description must be a string"))))

  rule)

(defun validate-fact (fact)
  "Validate fact structure"
  (unless (listp fact)
    (error 'invalid-fact-error
           :fact fact
           :reason "Fact must be a list"))

  (unless (>= (length fact) 1)
    (error 'invalid-fact-error
           :fact fact
           :reason "Fact must have at least one element"))

  (unless (symbolp (first fact))
    (error 'invalid-fact-error
           :fact fact
           :reason "First element of fact must be a symbol"))

  fact)

;; =============================================================================
;; ENHANCED SYSTEM FUNCTIONS WITH ERROR HANDLING
;; =============================================================================

;; Enhanced add-fact with validation
(defun safe-add-fact (fact cf)
  "Add a fact with comprehensive validation and error handling"
  (handler-case
      (progn
        (validate-fact fact)
        (validate-certainty-factor cf "fact addition")
        (add-fact fact cf)
        (format t "Added fact: ~A (CF: ~,2F)~%" fact cf))
    (certainty-factor-error (e)
      (format t "ERROR: ~A~%" e)
      (format t "Fact not added.~%"))
    (invalid-fact-error (e)
      (format t "ERROR: ~A~%" e)
      (format t "Fact not added.~%"))
    (error (e)
      (format t "Unexpected error adding fact: ~A~%" e)
      (format t "Fact not added.~%"))))

;; Enhanced define-rule with validation
(defun safe-define-rule (conclusion conditions cf &optional description)
  "Define a rule with comprehensive validation"
  (let ((rule (list conclusion conditions cf description)))
    (handler-case
        (progn
          (validate-rule rule)
          (define-rule conclusion conditions cf description)
          (format t "Defined rule: ~A -> ~A (CF: ~,2F)~%" conditions conclusion cf))
      (malformed-rule-error (e)
        (format t "ERROR: ~A~%" e)
        (format t "Rule not defined.~%"))
      (certainty-factor-error (e)
        (format t "ERROR: ~A~%" e)
        (format t "Rule not defined.~%"))
      (error (e)
        (format t "Unexpected error defining rule: ~A~%" e)
        (format t "Rule not defined.~%")))))

;; Enhanced prove-goal with error recovery
(defun safe-prove-goal (goal)
  "Prove a goal with error handling and recovery"
  (handler-case
      (prove-goal goal)
    (error (e)
      (format t "Error during goal proving: ~A~%" e)
      (format t "Returning uncertainty (CF: 0.0)~%")
      0.0)))

;; Enhanced system reset with validation
(defun safe-clear-facts ()
  "Clear all facts with confirmation"
  (let ((fact-count (hash-table-count *facts*)))
    (when (> fact-count 0)
      (format t "Clearing ~A facts... " fact-count))
    (clear-facts)
    (format t "Facts cleared.~%")))

;; =============================================================================
;; ROBUST INPUT PARSING
;; =============================================================================

(defun safe-read-certainty-factor (prompt &optional (default 0.8))
  "Safely read a certainty factor from user input"
  (format t "~A (default: ~,2F): " prompt default)
  (force-output)
  (let ((input (string-trim " " (read-line))))
    (if (string= input "")
        default
        (handler-case
            (let ((cf (read-from-string input)))
              (validate-certainty-factor cf "user input")
              cf)
          (certainty-factor-error (e)
            (format t "Invalid certainty factor. Using default ~,2F~%" default)
            default)
          (error (e)
            (format t "Invalid input. Using default ~,2F~%" default)
            default)))))

;; =============================================================================
;; SYSTEM HEALTH CHECKS
;; =============================================================================

(defun system-health-check ()
  "Perform comprehensive system health checks"
  (format t "~%=== SYSTEM HEALTH CHECK ===~%")

  (let ((valid-rules 0)
        (invalid-rules 0)
        (valid-facts 0)
        (invalid-facts 0))

    ;; Check rule integrity
    (dolist (rule *rules*)
      (handler-case
          (progn
            (validate-rule rule)
            (incf valid-rules))
        (error (e)
          (incf invalid-rules)
          (format t "INVALID RULE: ~A~%" rule))))
    (format t "Rules: ~A valid, ~A invalid~%" valid-rules invalid-rules)

    ;; Check fact integrity
    (maphash (lambda (fact cf)
               (handler-case
                   (progn
                     (validate-fact fact)
                     (validate-certainty-factor cf "fact validation")
                     (incf valid-facts))
                 (error (e)
                   (incf invalid-facts)
                   (format t "INVALID FACT: ~A (CF: ~A)~%" fact cf))))
             *facts*)
    (format t "Facts: ~A valid, ~A invalid~%" valid-facts invalid-facts)

    ;; Check system state
    (format t "System state: ~A rules loaded, ~A facts in memory~%"
            (length *rules*) (hash-table-count *facts*))

    (let ((total-issues (+ invalid-rules invalid-facts)))
      (if (= total-issues 0)
          (format t "✓ System health: EXCELLENT~%")
          (format t "⚠ System health: ISSUES FOUND (~A)~%" total-issues)))))

;; =============================================================================
;; ERROR RECOVERY UTILITIES
;; =============================================================================

(defun recover-from-invalid-rules ()
  "Remove invalid rules from the system"
  (format t "Recovering from invalid rules...~%")
  (let ((original-count (length *rules*))
        (valid-rules nil))
    (dolist (rule *rules*)
      (handler-case
          (progn
            (validate-rule rule)
            (push rule valid-rules))
        (error (e)
          (format t "Removing invalid rule: ~A~%" rule))))
    (setf *rules* (nreverse valid-rules))
    (format t "Recovery complete: ~A -> ~A rules~%"
            original-count (length *rules*))))

(defun recover-from-invalid-facts ()
  "Remove invalid facts from the system"
  (format t "Recovering from invalid facts...~%")
  (let ((original-count (hash-table-count *facts*)))
    (clrhash *facts*)
    (format t "Recovery complete: ~A -> 0 facts~%" original-count)))

;; =============================================================================
;; DEMONSTRATION OF ERROR HANDLING
;; =============================================================================

(defun demonstrate-error-handling ()
  "Demonstrate the error handling capabilities"
  (format t "~%=== ERROR HANDLING DEMONSTRATION ===~%")

  ;; Test invalid certainty factors
  (format t "~%1. Testing invalid certainty factors:~%")
  (safe-add-fact '(test-fact) 1.5)  ; Too high
  (safe-add-fact '(test-fact) -1.5) ; Too low
  (safe-add-fact '(test-fact) 0.8)  ; Valid

  ;; Test invalid facts
  (format t "~%2. Testing invalid facts:~%")
  (safe-add-fact "not-a-list" 0.8)
  (safe-add-fact '() 0.8)
  (safe-add-fact '(valid fact) 0.8)

  ;; Test invalid rules
  (format t "~%3. Testing invalid rules:~%")
  (safe-define-rule "not-a-list" '((condition)) 0.8)
  (safe-define-rule '(conclusion) "not-a-list" 0.8)
  (safe-define-rule '(conclusion) '((condition)) 1.5)
  (safe-define-rule '(conclusion) '((condition)) 0.8 "Valid rule")

  ;; System health check
  (system-health-check)

  (format t "~%Error handling demonstration complete.~%"))

;; =============================================================================
;; INTEGRATION WITH EXISTING SYSTEM
;; =============================================================================

;; Override the original add-fact to include validation
(let ((original-add-fact (symbol-function 'add-fact)))
  (defun add-fact (fact cf)
    "Enhanced add-fact with validation"
    (validate-fact fact)
    (validate-certainty-factor cf "fact addition")
    (funcall original-add-fact fact cf)))

;; Override define-rule to include validation
(let ((original-define-rule (symbol-function 'define-rule)))
  (defun define-rule (conclusion conditions cf &optional description)
    "Enhanced define-rule with validation"
    (validate-rule (list conclusion conditions cf description))
    (funcall original-define-rule conclusion conditions cf description)))

(format t "~%Error Handling Enhancements loaded!~%")
(format t "Type (demonstrate-error-handling) to see error handling in action~%")
(format t "Type (system-health-check) to check system integrity~%")