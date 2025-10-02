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
