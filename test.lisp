(println (+ 1 2 3))
(println '(+ 1 2 3))

(defmacro test-macro (lambda (code) (list 'quote (cdr code))))
(println (test-macro 1 2 3))


(defvar parse-lisp (lambda (str) 

    str))
(println (parse-lisp "(+ 1 2)"))