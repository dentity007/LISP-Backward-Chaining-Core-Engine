;; Simple Common Lisp Hello World Program
;; This demonstrates basic Lisp syntax and functions

(defun hello-world ()
  "A simple function that prints Hello, World!"
  (format t "Hello, World from Common Lisp!~%"))

(defun add-numbers (a b)
  "Function to add two numbers"
  (+ a b))

(defun factorial (n)
  "Calculate factorial recursively"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Example usage (uncomment to run)
;; (hello-world)
;; (format t "2 + 3 = ~a~%" (add-numbers 2 3))
;; (format t "5! = ~a~%" (factorial 5))