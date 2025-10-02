;;;; ASDF System Definition for the Car Expert System

(asdf:defsystem :car-expert-system
  :description "Backward-chaining car troubleshooting expert system (Common Lisp)"
  :author "Project Contributors"
  :license "MIT"
  :serial t
  :pathname "car-expert-system/"
  :components ((:file "expert-system")
               (:file "car-rules")
               (:file "run")))

(asdf:defsystem :car-expert-system/tests
  :description "Tests for the car expert system"
  :depends-on (:car-expert-system)
  :serial t
  :pathname "car-expert-system/"
  :components ((:file "certainty-factor-tests")
               (:file "comprehensive-tests")
               (:file "test")))

;; Integrate a simple, consistent test entry point for ASDF test-op
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :car-expert-system/tests))))
    (declare (ignore system))
    (asdf:load-system :car-expert-system/tests)
    (let* ((ok1 (uiop:symbol-call :expert-system :run-certainty-tests))
           (ok2 (uiop:symbol-call :expert-system :run-comprehensive-tests)))
      (unless (and ok1 ok2)
        (error "Expert system tests failed"))
      t)))

;; Allow testing via the main system name as well
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :car-expert-system))))
    (declare (ignore system))
    (asdf:load-system :car-expert-system/tests)
    (asdf:perform 'asdf:test-op (asdf:find-system :car-expert-system/tests))))
