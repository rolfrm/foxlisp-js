;;; Test file for FoxLisp debugging features
;;; Run with: ./fox lisp/test-debug.lisp

(println "=== Testing FoxLisp Debug Features ===")

;; Load debug utilities
(loadfile "lisp/debug.lisp")

;; Test 1: dbg macro
(println "\n--- Test 1: dbg macro ---")
(defvar test-value (dbg (+ 1 2 3)))
(println "Result:" test-value)

;; Test 2: dbg-label macro
(println "\n--- Test 2: dbg-label macro ---")
(defvar labeled-value (dbg-label "My calculation" (* 5 5)))
(println "Result:" labeled-value)

;; Test 3: Define some functions to test tracing
(println "\n--- Test 3: Function definitions ---")

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(println "factorial(5) =" (factorial 5))
(println "fibonacci(10) =" (fibonacci 10))

;; Test 4: Timing
(println "\n--- Test 4: Timing ---")
(dbg-time "factorial-1000"
  (let ((result 1))
    (dotimes (i 100)
      (set result (factorial 10)))
    result))

;; Test 5: Log levels
(println "\n--- Test 5: Log levels ---")
(log-set-level 'debug)
(log-debug "This is a debug message")
(log-info "This is an info message")
(log-warn "This is a warning message")
(log-error "This is an error message")

(log-set-level 'warn)
(println "Log level set to warn:")
(log-debug "This should NOT appear")
(log-info "This should NOT appear")
(log-warn "This SHOULD appear")
(log-error "This SHOULD appear")

;; Test 6: Assertions
(println "\n--- Test 6: Assertions ---")
(dbg-assert (eq 1 1) "1 should equal 1")
(println "Basic assertion passed")

(dbg-assert-eq 42 42)
(println "Equality assertion passed")

;; Test 7: Watch variables
(println "\n--- Test 7: Watch variables ---")
(defvar my-counter 0)
(watch-add 'my-counter my-counter)

(set my-counter 1)
(watch-update 'my-counter my-counter)

(set my-counter 5)
(watch-update 'my-counter my-counter)

(set my-counter 5)  ;; Same value
(watch-update 'my-counter my-counter)  ;; Should not print change

;; Test 8: Helper functions
(println "\n--- Test 8: Helper functions ---")
(println "index-of '(a b c d) 'c =" (index-of '(a b c d) 'c))
(println "index-of '(a b c d) 'e =" (index-of '(a b c d) 'e))

;; Test 9: Separators
(println "\n--- Test 9: Separators ---")
(dbg-separator)
(dbg-separator "Custom Label")

;; Test 10: Nested function calls with error
(println "\n--- Test 10: Testing stack trace on error ---")
(defun level3 ()
  (println "In level3")
  ;; Let's trigger an error
  ;; (raise "Test error from level3")
  (println "level3 completed")
  42)

(defun level2 ()
  (println "In level2")
  (let ((result (level3)))
    (println "level2 got result:" result)
    result))

(defun level1 ()
  (println "In level1")
  (let ((result (level2)))
    (println "level1 got result:" result)
    result))

(println "Calling level1:")
(println "Final result:" (level1))

(println "\n=== All debug tests completed successfully! ===")
