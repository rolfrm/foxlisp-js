;;; FoxLisp Debug Utilities
;;; Load this file to enable debugging features in your Lisp code

;; Debug mode flag
(defvar *debug-mode* nil)

;; Trace mode flag
(defvar *trace-mode* nil)

;; Reference to the debug module (set at runtime)
(defvar __debug_module nil)

;; Initialize debug module reference (for Node.js environments)
(defun debug-init ()
  "Initialize the debug module reference"
  (set __debug_module (%js "typeof __foxlisp_debug !== 'undefined' ? __foxlisp_debug : null"))
  __debug_module)

;;; ============================================================
;;; Debug Configuration
;;; ============================================================

;; Enable debug mode
(defun debug-enable ()
  "Enable debug mode for detailed error reporting"
  (set *debug-mode* t)
  (when __debug_module
    (%js "__debug_module.debugConfig.enabled = true"))
  (println "Debug mode enabled"))

;; Disable debug mode
(defun debug-disable ()
  "Disable debug mode"
  (set *debug-mode* nil)
  (when __debug_module
    (%js "__debug_module.debugConfig.enabled = false"))
  (println "Debug mode disabled"))

;; Enable function tracing
(defun trace-enable ()
  "Enable function call tracing"
  (set *trace-mode* t)
  (when __debug_module
    (%js "__debug_module.debugConfig.traceEnabled = true"))
  (println "Trace mode enabled"))

;; Disable function tracing
(defun trace-disable ()
  "Disable function call tracing"
  (set *trace-mode* nil)
  (when __debug_module
    (%js "__debug_module.debugConfig.traceEnabled = false"))
  (println "Trace mode disabled"))

;;; ============================================================
;;; Breakpoints
;;; ============================================================

;; Add a breakpoint at file:line
(defun breakpoint-add (file line)
  "Add a breakpoint at the specified file and line number"
  (when __debug_module
    (%js "__debug_module.addBreakpoint(file, line)"))
  (println "Breakpoint added:" file ":" line))

;; Remove a breakpoint
(defun breakpoint-remove (file line)
  "Remove a breakpoint from the specified location"
  (when __debug_module
    (%js "__debug_module.removeBreakpoint(file, line)"))
  (println "Breakpoint removed:" file ":" line))

;; Clear all breakpoints
(defun breakpoint-clear ()
  "Remove all breakpoints"
  (when __debug_module
    (%js "__debug_module.clearBreakpoints()"))
  (println "All breakpoints cleared"))

;; List all breakpoints
(defun breakpoint-list ()
  "List all active breakpoints"
  (when __debug_module
    (%js "console.log('Breakpoints:', __debug_module.listBreakpoints())")))

;;; ============================================================
;;; Stack Trace Utilities
;;; ============================================================

;; Print the current call stack
(defun print-stack ()
  "Print the current Lisp call stack"
  (%js "__debug_print_stack && __debug_print_stack()"))

;; Get the current call stack as a list
(defun get-stack ()
  "Return the current call stack"
  (%js "__debug_stack ? __debug_stack() : []"))

;;; ============================================================
;;; Inspection Utilities
;;; ============================================================

;; Inspect a value
(defun dbg-inspect (name value)
  "Inspect a value and print detailed information about it"
  (%js "__debug_inspect && __debug_inspect(name, value)")
  value)

;; Format a value for display
(defun dbg-format (value)
  "Format a value as a string for debugging"
  (if (%js "typeof __debug_format === 'function'")
      (%js "__debug_format(value)")
      (__valueToString value)))

;;; ============================================================
;;; Debug Macros
;;; ============================================================

;; Debug print macro - prints expression and its value
(defmacro dbg (expr)
  "Print an expression and its value, then return the value"
  `(let ((dbg-result ,expr))
     (println ,(concat "DBG: " (__valueToString expr) " =>") dbg-result)
     dbg-result))

;; Debug print with custom label
(defmacro dbg-label (label expr)
  "Print an expression with a custom label, then return the value"
  `(let ((dbg-result ,expr))
     (println ,label "=>" dbg-result)
     dbg-result))

;; Conditional breakpoint macro
(defmacro dbg-break-if (condition)
  "Trigger a debugger breakpoint if condition is true"
  `(when ,condition
     (%js "debugger")
     (print-stack)))

;; Unconditional breakpoint
(defmacro dbg-break ()
  "Trigger a debugger breakpoint"
  `(progn
     (%js "debugger")
     (print-stack)))

;; Trace a function call
(defmacro dbg-trace (name args &rest body)
  "Define a function with automatic entry/exit tracing"
  `(defun ,name ,args
     (println "ENTER:" ',name "with args:" (list ,@args))
     (let ((trace-result (progn ,@body)))
       (println "EXIT:" ',name "=>" trace-result)
       trace-result)))

;;; ============================================================
;;; Timing and Profiling
;;; ============================================================

;; Simple timer storage
(defvar *timers* (makehashmap))

;; Start a timer
(defun timer-start (name)
  "Start a named timer for profiling"
  (let ((start-time (%js "typeof performance !== 'undefined' ? performance.now() : Date.now()")))
    (hashmap-set *timers* name (list start-time 0 0))))

;; End a timer and return elapsed time
(defun timer-end (name)
  "End a named timer and return the elapsed time in milliseconds"
  (let ((timer-data (hashmap-get *timers* name)))
    (if timer-data
        (let ((start-time (nth timer-data 0))
              (end-time (%js "typeof performance !== 'undefined' ? performance.now() : Date.now()")))
          (let ((elapsed (- end-time start-time)))
            (hashmap-set *timers* name (list start-time elapsed (+ (nth timer-data 2) 1)))
            elapsed))
        0)))

;; Print timer report
(defun timer-report ()
  "Print a report of all timer measurements"
  (println "[Timer Report]")
  ;; Note: would need proper hashmap iteration
  *timers*)

;; Clear all timers
(defun timer-clear ()
  "Clear all timer measurements"
  (set *timers* (makehashmap)))

;; Time a block of code
(defmacro dbg-time (name &rest body)
  "Execute body and print the execution time"
  `(progn
     (timer-start ,name)
     (let ((time-result (progn ,@body)))
       (let ((elapsed (timer-end ,name)))
         (println ,name "took" elapsed "ms")
         time-result))))

;;; ============================================================
;;; Assertion Enhancements
;;; ============================================================

;; Enhanced assert with debug info
(defmacro dbg-assert (condition &rest message)
  "Assert a condition with optional message, printing debug info on failure"
  `(unless ,condition
     (println "ASSERTION FAILED:" ',condition)
     (when (list? '(,@message))
       (println "Message:" ,@message))
     (print-stack)
     (raise (list "assertion failed" ',condition ,@message))))

;; Assert with value inspection
(defmacro dbg-assert-eq (expected actual)
  "Assert two values are equal, printing both on failure"
  `(let ((exp-val ,expected)
         (act-val ,actual))
     (unless (eq exp-val act-val)
       (println "ASSERTION FAILED: values not equal")
       (println "  Expected:" exp-val)
       (println "  Actual:  " act-val)
       (print-stack)
       (raise (list "assert-eq failed" exp-val act-val)))))

;;; ============================================================
;;; Watch Variables
;;; ============================================================

;; Storage for watched variables
(defvar *watched-vars* (makehashmap))

;; Watch a variable
(defun watch-add (name value)
  "Add a variable to the watch list"
  (hashmap-set *watched-vars* name value)
  (println "Watching:" name "=" (dbg-format value)))

;; Update a watched variable
(defun watch-update (name value)
  "Update a watched variable and show if it changed"
  (let ((old-value (hashmap-get *watched-vars* name)))
    (unless (eq old-value value)
      (println "WATCH:" name "changed from" (dbg-format old-value) "to" (dbg-format value))
      (hashmap-set *watched-vars* name value)))
  value)

;; Remove a watch
(defun watch-remove (name)
  "Remove a variable from the watch list"
  (hashmap-set *watched-vars* name nil)
  (println "Stopped watching:" name))

;; Show all watched variables
(defun watch-show ()
  "Display all watched variables"
  (println "Watched variables:")
  ;; Note: would need hashmap iteration support
  *watched-vars*)

;; Macro to automatically watch a variable on set
(defmacro dbg-watch (var value)
  "Set a variable and track changes to it"
  `(progn
     (set ,var ,value)
     (watch-update ',var ,var)
     ,var))

;;; ============================================================
;;; Log Levels
;;; ============================================================

(defvar *log-level* 'info)  ;; 'debug, 'info, 'warn, 'error

(defun log-set-level (level)
  "Set the log level: debug, info, warn, or error"
  (set *log-level* level))

(defun log-should-print (level)
  "Check if we should print at this log level"
  (let ((levels '(debug info warn error)))
    (let ((current-idx (index-of levels *log-level*))
          (msg-idx (index-of levels level)))
      ;; Note: need to check for nil explicitly since 0 is falsy in JS
      (and (not (null? msg-idx))
           (not (null? current-idx))
           (>= msg-idx current-idx)))))

(defmacro log-debug (&rest args)
  "Log a debug message"
  `(when (log-should-print 'debug)
     (println "[DEBUG]" ,@args)))

(defmacro log-info (&rest args)
  "Log an info message"
  `(when (log-should-print 'info)
     (println "[INFO]" ,@args)))

(defmacro log-warn (&rest args)
  "Log a warning message"
  `(when (log-should-print 'warn)
     (println "[WARN]" ,@args)))

(defmacro log-error (&rest args)
  "Log an error message"
  `(when (log-should-print 'error)
     (println "[ERROR]" ,@args)))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;; Find index of item in list
(defun index-of (lst item)
  "Find the index of an item in a list"
  (let ((idx 0)
        (len (length lst))
        (found -1))
    (loop (and (< idx len) (< found 0))
      (if (eq (nth lst idx) item)
          (set found idx)
          (set idx (+ idx 1))))
    (if (< found 0) nil found)))

;; Print a separator line
(defun dbg-separator (&rest label)
  "Print a separator line for visual debugging"
  (if (and label (car label))
      (println "========" (car label) "========")
      (println "==========================")))

;; Dump the structure of an object
(defun dbg-dump (obj &rest depth)
  "Dump the structure of an object recursively"
  (let ((d (if depth (car depth) 0))
        (indent (make-string (* d 2) " ")))
    (cond
      ((null? obj) (println indent "nil"))
      ((list? obj)
       (println indent "(")
       (foreach elem obj
         (dbg-dump elem (+ d 1)))
       (println indent ")"))
      ((object? obj)
       (println indent "{")
       (foreach-key key obj
         (println indent "  " key ":")
         (dbg-dump (get obj key) (+ d 1)))
       (println indent "}"))
      (t (println indent (dbg-format obj))))))

;; Create a string of repeated characters
(defun make-string (n char)
  "Create a string of n repeated characters"
  (let ((result ""))
    (dotimes (i n)
      (set result (concat result char)))
    result))

;; Iterate over object keys (helper)
(defmacro foreach-key (var obj &rest body)
  "Iterate over the keys of an object"
  `(let ((keys (%js "Object.keys(" ,obj ")")))
     (foreach ,var keys ,@body)))

(println "FoxLisp debug utilities loaded. Use (debug-enable) to start debugging.")
