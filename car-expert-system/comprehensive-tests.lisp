;; =============================================================================
;; COMPREHENSIVE-TESTS.LISP
;; Scenario tests for backward-chaining car expert system
;; =============================================================================

(load "expert-system.lisp")
(load "car-rules.lisp")
(in-package :expert-system)

(defvar *ct-pass* 0)
(defvar *ct-fail* 0)

;; Disable interactive prompts during automated tests
(setf *interactive-questions* nil)

(defmacro deftest (name &body body)
  `(defun ,name ()
     (format t "~%[TEST] ~A ... " ',name)
     (handler-case
         (let ((ok (progn ,@body)))
           (if ok
               (progn (incf *ct-pass*) (format t "PASS"))
               (progn (incf *ct-fail*) (format t "FAIL"))))
       (error (e)
         (incf *ct-fail*)
         (format t "ERROR: ~A" e)))))

(defun approx>= (x y &optional (tol 0.01))
  (>= (+ x tol) y))

;; Dead battery should be likely with does-not-start + lights dim
(deftest dead-battery-likely
  (clear-session)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (let ((cf (prove-goal '(car-problem dead-battery))))
    (and (certainty-true-p cf) (approx>= cf 0.6))))

;; Overheating should be likely with engine running + high temp + low coolant
(deftest overheating-likely
  (clear-session)
  (add-fact '(engine running) 0.9)
  (add-fact '(temperature high) 0.9)
  (add-fact '(coolant low) 0.8)
  (let ((cf (prove-goal '(car-problem overheating))))
    (certainty-true-p cf)))

;; Brake-system likely with brake warning light
(deftest brake-system-likely
  (clear-session)
  (add-fact '(brake-warning-light on) 0.95)
  (let ((cf (prove-goal '(car-problem brake-system))))
    (certainty-true-p cf)))

;; Starter failure unlikely if lights are dim (contradicting condition)
(deftest starter-failure-unlikely
  (clear-session)
  (add-fact '(car does-not-start) 0.9)
  (add-fact '(lights dim-or-off) 0.8)
  (let ((cf (prove-goal '(car-problem starter-failure))))
    (not (certainty-true-p cf))))

(defun run-comprehensive-tests ()
  (setf *ct-pass* 0 *ct-fail* 0)
  (format t "~%========================================~%")
  (format t "   COMPREHENSIVE EXPERT SYSTEM TESTS~%")
  (format t "========================================~%")
  (dead-battery-likely)
  (overheating-likely)
  (brake-system-likely)
  (starter-failure-unlikely)
  (format t "~%~%Summary: ~A passed, ~A failed~%" *ct-pass* *ct-fail*)
  (= *ct-fail* 0))

(format t "~%Comprehensive tests loaded. Run (run-comprehensive-tests).~%")
