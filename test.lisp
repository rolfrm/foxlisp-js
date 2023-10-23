(defvar make-map makemap_)
(println (+ 1 2 3))
(println '(+ 1 2 3))

(defmacro test-macro (lambda (code) (list 'quote (cdr code))))
(println (test-macro 1 2 3))

(defvar space (car " "))

(defvar skip-whitespace (lambda (str) 
    (loop (eq space (car str))
       (set str (cdr str)))
    str
))


(defvar parse-lisp (lambda (str) 

    (car str)))
(println (parse-lisp "(+ 1 2)"))
(defvar t (eq 1 1))
(defvar false (eq 1 0))

(println (skip-whitespace "  123"))
(defvar obj2 (makemap_))
(set obj2 (makemap_))
(println obj2)
(put obj2 'x 5)
(println "obj2:" obj2 (get obj2 'x))
;(if  (println 'yes) (println 'no))