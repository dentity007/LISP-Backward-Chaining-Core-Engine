;;;; Backward Chaining Expert System with Certainty Factors
;;;; Based on MYCIN-style reasoning with uncertainty handling

;;; Package definition
(defpackage :expert-system
  (:use :common-lisp)
  (:export #:define-rule
           #:ask-question
           #:consult
           #:clear-session
           #:show-facts
           #:enable-trace
           #:disable-trace
           #:*certainty-threshold*))

(in-package :expert-system)

;;; Global variables
(defvar *facts* (make-hash-table :test 'equal)
  "Hash table storing facts and their certainty factors")

(defvar *rules* nil
  "List of rules in the expert system")

(defvar *asked-questions* (make-hash-table :test 'equal)
  "Hash table tracking which questions have been asked")

(defvar *trace-enabled* nil
  "Enable tracing of inference process")

(defvar *certainty-threshold* 0.2
  "Minimum certainty factor for a fact to be considered true")

;;; Certainty factor operations
(defun combine-certainty (cf1 cf2)
  "Combine two certainty factors using MYCIN's formula"
  (cond
    ((and (>= cf1 0) (>= cf2 0))
     (+ cf1 cf2 (* -1 cf1 cf2)))
    ((and (< cf1 0) (< cf2 0))
     (+ cf1 cf2 (* cf1 cf2)))
    (t
     (/ (+ cf1 cf2)
        (- 1 (min (abs cf1) (abs cf2)))))))

(defun certainty-true-p (cf)
  "Check if certainty factor indicates truth"
  (>= cf *certainty-threshold*))

(defun certainty-false-p (cf)
  "Check if certainty factor indicates falsehood"
  (<= cf (- *certainty-threshold*)))

;;; Fact management
(defun add-fact (fact cf)
  "Add or update a fact with its certainty factor"
  (let ((existing-cf (gethash fact *facts* 0)))
    (setf (gethash fact *facts*) 
          (combine-certainty existing-cf cf))
    (when *trace-enabled*
      (format t "~&Added fact: ~A with CF: ~,2F~%" fact (gethash fact *facts*)))
    (gethash fact *facts*)))

(defun get-fact-cf (fact)
  "Get the certainty factor for a fact"
  (gethash fact *facts* 0))

(defun fact-known-p (fact)
  "Check if a fact is known (has been assigned a certainty factor)"
  (multiple-value-bind (value exists-p)
      (gethash fact *facts*)
    (declare (ignore value))
    exists-p))

(defun clear-facts ()
  "Clear all facts and asked questions"
  (clrhash *facts*)
  (clrhash *asked-questions*)
  (when *trace-enabled*
    (format t "~&Cleared all facts and questions~%")))

;;; Rule structure and management
(defstruct rule
  name
  goal
  conditions
  cf
  question)

(defmacro define-rule (name goal conditions cf &optional question)
  "Define a rule for backward chaining"
  `(progn
     (setf *rules* (remove ',name *rules* :key #'rule-name))
     (push (make-rule :name ',name
                      :goal ',goal
                      :conditions ',conditions
                      :cf ,cf
                      :question ,question)
           *rules*)
     ',name))

(defun clear-rules ()
  "Clear all rules"
  (setf *rules* nil))

;;; Question asking mechanism
(defun ask-question (fact question)
  "Ask a question about a fact and store the answer"
  (when (gethash fact *asked-questions*)
    (return-from ask-question (get-fact-cf fact)))
  
  (setf (gethash fact *asked-questions*) t)
  
  (format t "~&~A~%" question)
  (format t "Enter certainty (-1 to 1, or y/n for 0.8/-0.8): ")
  (force-output)
  
  (let ((input (string-trim " " (read-line))))
    (cond
      ((string-equal input "y") (add-fact fact 0.8))
      ((string-equal input "yes") (add-fact fact 0.8))
      ((string-equal input "n") (add-fact fact -0.8))
      ((string-equal input "no") (add-fact fact -0.8))
      ((string-equal input "u") (add-fact fact 0))
      ((string-equal input "unknown") (add-fact fact 0))
      (t (let ((cf (ignore-errors (read-from-string input))))
           (if (and cf (numberp cf) (>= cf -1) (<= cf 1))
               (add-fact fact cf)
               (progn
                 (format t "Invalid input. Using 0 (unknown).~%")
                 (add-fact fact 0))))))))

;;; Backward chaining inference
(defun prove-goal (goal)
  "Attempt to prove a goal using backward chaining"
  (when *trace-enabled*
    (format t "~&Trying to prove: ~A~%" goal))
  
  ;; If fact is already known, return its certainty factor
  (when (fact-known-p goal)
    (when *trace-enabled*
      (format t "~&Goal ~A already known with CF: ~,2F~%" goal (get-fact-cf goal)))
    (return-from prove-goal (get-fact-cf goal)))
  
  ;; Try to prove using rules
  (let ((max-cf 0))
    (dolist (rule *rules*)
      (when (equal (rule-goal rule) goal)
        (when *trace-enabled*
          (format t "~&Trying rule: ~A~%" (rule-name rule)))
        
        (let ((conditions-cf (prove-conditions (rule-conditions rule))))
          (when (certainty-true-p conditions-cf)
            (let ((rule-cf (* conditions-cf (rule-cf rule))))
              (when *trace-enabled*
                (format t "~&Rule ~A succeeded with CF: ~,2F~%" (rule-name rule) rule-cf))
              (setf max-cf (combine-certainty max-cf rule-cf)))))))
    
    ;; If no rules succeeded and we have a question, ask it
    (when (and (= max-cf 0) (not (fact-known-p goal)))
      (let ((question-rule (find-if (lambda (r) 
                                      (and (equal (rule-goal r) goal)
                                           (rule-question r)))
                                    *rules*)))
        (when question-rule
          (setf max-cf (ask-question goal (rule-question question-rule))))))
    
    ;; Store the result
    (when (> (abs max-cf) 0)
      (add-fact goal max-cf))
    
    max-cf))

(defun prove-conditions (conditions)
  "Prove all conditions and return minimum certainty factor"
  (if (null conditions)
      1.0
      (let ((min-cf 1.0))
        (dolist (condition conditions)
          (let ((cf (prove-goal condition)))
            (when *trace-enabled*
              (format t "~&Condition ~A has CF: ~,2F~%" condition cf))
            (setf min-cf (min min-cf cf))))
        min-cf)))

;;; Main consultation interface
(defun consult (goal)
  "Start a consultation to prove a goal"
  (format t "~&Starting consultation for: ~A~%" goal)
  (let ((cf (prove-goal goal)))
    (format t "~&~%Consultation complete.~%")
    (format t "Goal: ~A~%" goal)
    (format t "Certainty Factor: ~,2F~%" cf)
    (cond
      ((certainty-true-p cf)
       (format t "Conclusion: TRUE (confidence: ~,1F%)~%" (* cf 100)))
      ((certainty-false-p cf)
       (format t "Conclusion: FALSE (confidence: ~,1F%)~%" (* (abs cf) 100)))
      (t
       (format t "Conclusion: UNKNOWN (insufficient evidence)~%")))
    cf))

(defun clear-session ()
  "Clear facts and asked questions for a new consultation"
  (clear-facts)
  (when *trace-enabled*
    (format t "~&Session cleared. Ready for new consultation.~%")))

;;; Utility functions
(defun show-facts ()
  "Display all current facts and their certainty factors"
  (format t "~&Current facts:~%")
  (maphash (lambda (fact cf)
             (format t "  ~A: ~,2F~%" fact cf))
           *facts*)
  (format t "~&Total: ~A facts~%" (hash-table-count *facts*)))

(defun enable-trace ()
  "Enable tracing of inference process"
  (setf *trace-enabled* t)
  (format t "~&Tracing enabled~%"))

(defun disable-trace ()
  "Disable tracing of inference process"
  (setf *trace-enabled* nil)
  (format t "~&Tracing disabled~%"))

;;; Demo function
(defun demo-backward-chaining ()
  "Demonstrate backward chaining with simple rules"
  (clear-session)
  (clear-rules)
  
  ;; Define some simple rules
  (define-rule mortal-rule
    '(mortal ?x)
    '((human ?x))
    0.9)
  
  (define-rule human-rule
    '(human socrates)
    '()
    1.0
    "Is Socrates human? (y/n)")
  
  (enable-trace)
  (format t "~&Demo: Proving that Socrates is mortal~%")
  (consult '(mortal socrates)))

(format t "~&Backward chaining expert system loaded successfully.~%")