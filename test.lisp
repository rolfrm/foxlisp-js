(defvar make-map makemap_)
(defvar parse-integer parseInt)
(defvar parse-float parseFloat)
(defvar >= gte)
(defvar <= lte)
(println (+ 1 2 3))
(println '(+ 1 2 3))

(defmacro test-macro (lambda (code) (list 'quote (cdr code))))
(println (test-macro 1 2 3))

(defvar space (car " "))
(defvar tab (car "	"))
(defvar newline (car "
"))
(defvar paren-start (car "("))
(defvar paren-end (car ")"))
(defvar is-whitespace (lambda (front) (or (eq space front) (eq tab front) (eq newline front))))
(defvar skip-whitespace (lambda (str) 
    (loop (let ((front (car str))) (is-whitespace front))
       (set str (cdr str)))
    str
))


;; (defmacro case (lambda (cases)
;;     (let ((value (car cases))
;;           (out-cases (list)))
;;         (let ((cases (cdr cases)))
;;             (loop cases 
;;                 (let ((thiscase (car cases)))
                    
;;                     (list 'if (list 'eq value (list car case)))
;;                 )
;;                 (set cases (cdr cases))
;;             )
        
;;         )
;;     ))
;; )
(defvar t (eq 1 1))
(defvar false (eq 1 0))
(defvar minus-char (car "-"))
(defvar plus-char (car "+"))
(defvar char-0 (car "0"))
(defvar char-9 (car "9"))
(defvar char-dot (car "."))
(defvar sym-end (lambda (x) (or (is-whitespace x) (eq paren-start x)
										  (eq paren-end x))))

;(Defvar is-digit (lambda (x) (

(defvar parse-number
  (lambda (input)
	 (block result
        (let ((negative (eq (car input) minus-char))
              (is-float 0)
				  (final-parse (lambda (x) (if is-float (parse-float x)
														 (parse-integer x))))
				  (output ""))
          (if negative (set input (cdr input))
                (if (eq plus-char (car input))
                    (set input (cdr input))
                )
            )
          (loop input 
					 (let ((fst (car input)))
						(if (and (>= fst char-0) (<= fst char-9))
							 (set output (+ output fst))
							 (if (and (not is-float) char-dot)
								  (progn
									 (set is-float t)
									 (set output (+ output fst)))
								  (if (sym-end fst)
										(return-from result (list (final-parse
																			output) input ))
										(return-from result null)))))
							 
						
					 (set input (cdr input))
					 )
			 
			 (if output (return-from result (list (final-parse output) input))
				  (return-from result ()))
			 ))))



(defvar parse-lisp (lambda (str) 
    (block finish
      (loop str 
        (set str (skip-whitespace str))
        (return-from finish 0)
        (let ((next (car str)))
            (if (eq next paren-start)

            1 2)
        
        )

    ))))
    
(println (parse-lisp "(+ 1 2)"))

(println (skip-whitespace " abc123__"))
(defvar obj2 (makemap_))
(set obj2 (makemap_))
(println obj2)  
(put obj2 'x 5)
(println "obj2:" obj2 (get obj2 'x))
(put obj2 "y" 3)
(println "obj2:" obj2 (get obj2 "y"))
(println 'newline: newline tab (charcode tab) "a")
(println "|" (strfromchar 31 32 33 34 9 9 9 9 35 36 37 38 39 40 41 42 43 44 52 43 54 (charcode (car "a"))) "|")

(println "r:" (or 0 2 3) "r2:" (and 1 2 3))

(println (skip-whitespace "    

asd"))

(println (block a (+ 1 2 (return-from a 5))))
(println (reverse (list 1 2 3)))
(println ())
(println "???")
(println "parse number: " (parse-number "123.5 asd"))
;(if  (println 'yes) (println 'no))
