;; Simple Common Lisp Hello World Program with execution

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

;; Execute the functions
(hello-world)
(format t "2 + 3 = ~a~%" (add-numbers 2 3))
(format t "5! = ~a~%" (factorial 5))