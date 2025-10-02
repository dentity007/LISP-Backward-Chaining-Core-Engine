;; =============================================================================
;; PERFORMANCE-BENCHMARKS.LISP
;; Performance benchmarks for the backward chaining expert system
;; Measures inference speed, rule processing efficiency, and memory usage
;; =============================================================================

(load "expert-system.lisp")
(load "car-rules.lisp")
(in-package :expert-system)

;; Benchmark utilities
(defvar *benchmark-results* nil)

(defun %dynamic-usage ()
  "Return dynamic memory usage if available for this Lisp."
  #+sbcl (sb-kernel:dynamic-usage)
  #-sbcl 0)

(defmacro time-operation (name &body body)
  "Time an operation and record the results"
  `(let ((start-time (get-internal-real-time))
         (start-bytes (%dynamic-usage)))
     (multiple-value-prog1
         (progn ,@body)
       (let ((end-time (get-internal-real-time))
             (end-bytes (%dynamic-usage)))
         (push (list ,name
                     (/ (- end-time start-time) internal-time-units-per-second)
                     (- end-bytes start-bytes))
               *benchmark-results*)))))

(defun print-benchmark-results ()
  "Print formatted benchmark results"
  (format t "~%========================================~%")
  (format t "    PERFORMANCE BENCHMARK RESULTS~%")
  (format t "========================================~%")
  (format t "~30A ~10A ~12A~%" "Operation" "Time (s)" "Memory (KB)")
  (format t "~30A ~10A ~12A~%" (make-string 30 :initial-element #\-)
          (make-string 10 :initial-element #\-)
          (make-string 12 :initial-element #\-))
  (dolist (result (reverse *benchmark-results*))
    (destructuring-bind (name time memory) result
      (format t "~30A ~10,3F ~12,D~%"
              name time (floor memory 1024))))
  (format t "~%"))

;; =============================================================================
;; SYSTEM LOADING BENCHMARKS
;; =============================================================================

(defun benchmark-system-loading ()
  "Benchmark system loading time"
  (format t "~%=== SYSTEM LOADING BENCHMARKS ===~%")

  ;; Clear any existing state
  (clrhash *facts*)
  (setf *rules* nil)

  ;; Benchmark expert system loading
  (time-operation "Load expert-system.lisp"
    (load "expert-system.lisp" :verbose nil))

  ;; Benchmark rules loading
  (time-operation "Load car-rules.lisp"
    (load "car-rules.lisp" :verbose nil))

  (format t "System loaded: ~A rules, ~A facts capacity~%"
          (length *rules*) (hash-table-size *facts*)))

;; =============================================================================
;; INFERENCE SPEED BENCHMARKS
;; =============================================================================

(defun benchmark-single-inferences ()
  "Benchmark individual backward chaining queries"
  (format t "~%=== SINGLE INFERENCE BENCHMARKS ===~%")

  ;; Test each major car problem
  (let ((test-cases '(("Dead Battery" (car-problem dead-battery)
                       ((car does-not-start) (lights dim-or-off)))
                      ("Starter Failure" (car-problem starter-failure)
                       ((car does-not-start) (lights work) (clicking-sound)))
                      ("Fuel System" (car-problem fuel-system)
                       ((car starts) (engine does-not-run)))
                      ("Ignition System" (car-problem ignition-system)
                       ((car starts) (engine does-not-run)))
                      ("Overheating" (car-problem overheating)
                       ((engine running) (temperature high) (coolant low)))
                      ("Brake System" (car-problem brake-system)
                       ((brake-warning-light on)))
                      ("Transmission" (car-problem transmission-slip)
                       ((car starts) (engine revs-high) (acceleration poor))))))

    (dolist (test-case test-cases)
      (destructuring-bind (name goal facts) test-case
        ;; Clear facts and add test facts
        (clear-facts)
        (dolist (fact facts)
          (add-fact fact 0.8))

        ;; Time the inference
        (time-operation (format nil "~A inference" name)
          (prove-goal goal))))))

;; =============================================================================
;; BATCH PROCESSING BENCHMARKS
;; =============================================================================

(defun benchmark-batch-inferences ()
  "Benchmark processing multiple queries in batch"
  (format t "~%=== BATCH INFERENCE BENCHMARKS ===~%")

  ;; Test different batch sizes
  (let ((batch-sizes '(5 10 20 50)))
    (dolist (batch-size batch-sizes)
      (clear-facts)
      ;; Add some base facts
      (add-fact '(car does-not-start) 0.8)
      (add-fact '(lights dim-or-off) 0.7)

      (time-operation (format nil "~A batch inferences" batch-size)
        (dotimes (i batch-size)
          (prove-goal '(car-problem dead-battery)))))))

;; =============================================================================
;; RULE PROCESSING EFFICIENCY BENCHMARKS
;; =============================================================================

(defun benchmark-rule-efficiency ()
  "Benchmark rule processing efficiency"
  (format t "~%=== RULE PROCESSING EFFICIENCY ===~%")

  ;; Count rules by complexity
  (let ((simple-rules 0)
        (complex-rules 0)
        (total-conditions 0))
    (dolist (rule *rules*)
      (let ((conditions (rule-conditions rule)))
        (incf total-conditions (length conditions))
        (if (<= (length conditions) 2)
            (incf simple-rules)
            (incf complex-rules))))

    (format t "Rule Statistics:~%")
    (format t "  Total rules: ~A~%" (length *rules*))
    (format t "  Simple rules (≤2 conditions): ~A~%" simple-rules)
    (format t "  Complex rules (>2 conditions): ~A~%" complex-rules)
    (format t "  Average conditions per rule: ~,2F~%"
            (/ total-conditions (length *rules*)))

    ;; Benchmark rule matching efficiency
    (clear-facts)
    (add-fact '(car does-not-start) 0.9)
    (add-fact '(lights dim-or-off) 0.8)
    (add-fact '(engine running) 0.7)

    (time-operation "Rule matching (3 facts)"
      (dolist (rule *rules*)
        (prove-conditions (rule-conditions rule))))))

;; =============================================================================
;; MEMORY USAGE BENCHMARKS
;; =============================================================================

(defun benchmark-memory-usage ()
  "Benchmark memory usage patterns"
  (format t "~%=== MEMORY USAGE BENCHMARKS ===~%")

  ;; Measure memory impact of adding facts
  (clear-facts)
  (let ((initial-usage (%dynamic-usage)))
    (time-operation "Add 10 facts"
      (dotimes (i 10)
        (add-fact (list 'test-fact i) 0.8)))

    (let ((with-facts-usage (%dynamic-usage)))
      (format t "Memory per fact: ~,1F KB~%"
              (/ (- with-facts-usage initial-usage) 10.0 1024))))

  ;; Measure memory impact of rules
  (format t "Rule storage: ~,1F KB for ~A rules~%"
          (/ (%dynamic-usage) 1024.0) (length *rules*)))

;; =============================================================================
;; SCALABILITY BENCHMARKS
;; =============================================================================

(defun benchmark-scalability ()
  "Benchmark system scalability"
  (format t "~%=== SCALABILITY BENCHMARKS ===~%")

  ;; Test with increasing numbers of facts
  (let ((fact-counts '(10 50 100)))
    (dolist (count fact-counts)
      (clear-facts)
      (dotimes (i count)
        (add-fact (list 'scalability-fact i) 0.8))

      (time-operation (format nil "Query with ~A facts" count)
        (prove-goal '(car-problem dead-battery))))))

;; =============================================================================
;; RUN ALL BENCHMARKS
;; =============================================================================

(defun run-performance-benchmarks ()
  "Run all performance benchmarks"
  (format t "~%========================================~%")
  (format t "    EXPERT SYSTEM PERFORMANCE BENCHMARKS~%")
  (format t "========================================~%")

  ;; Clear previous results
  (setf *benchmark-results* nil)

  ;; Run all benchmark categories
  (benchmark-system-loading)
  (benchmark-single-inferences)
  (benchmark-batch-inferences)
  (benchmark-rule-efficiency)
  (benchmark-memory-usage)
  (benchmark-scalability)

  ;; Print comprehensive results
  (print-benchmark-results)

  ;; Performance analysis
  (format t "~%=== PERFORMANCE ANALYSIS ===~%")
  (let ((total-time (reduce #'+ (mapcar #'second *benchmark-results*)))
        (inference-times (remove-if-not
                         (lambda (result) (search "inference" (string-downcase (first result))))
                         *benchmark-results*)))
    (format t "Total benchmark time: ~,3F seconds~%" total-time)
    (when inference-times
      (let ((avg-inference-time (/ (reduce #'+ (mapcar #'second inference-times))
                                   (length inference-times))))
        (format t "Average inference time: ~,4F seconds~%" avg-inference-time)
        (format t "Inferences per second: ~,1F~%" (/ 1 avg-inference-time)))))

  (format t "~%Benchmarks completed successfully!~%"))

;; Run benchmarks automatically when loaded
(format t "~%Performance Benchmarks loaded!~%")
(format t "Type (run-performance-benchmarks) to run all benchmarks~%")
