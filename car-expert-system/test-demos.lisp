;;;; Quick test of updated demo files
(load "simple-demo.lisp")

;; Test the quick battery test
(quick-battery-test)

;; Test the overheating test  
(quick-overheating-test)

;; Test starter failure
(quick-starter-test)