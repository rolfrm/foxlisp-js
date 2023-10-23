(println (+ 1 2 3))
(println '(+ 1 2 3))

(defmacro test-macro (lambda (code) (list 'quote (cdr code))))
(println (test-macro 1 2 3))

(defvar skip-whitespace (lambda (str) 
    

))


(defvar parse-lisp (lambda (str) 

    (car str)))
(println (parse-lisp "(+ 1 2)"))
(defvar t (eq 1 1))
(defvar false (eq 1 0))
;(if  (println 'yes) (println 'no))