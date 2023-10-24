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
(defvar nil ())
(defvar sym-end (lambda (x) (or (is-whitespace x) (eq paren-start x)
										  (eq paren-end x))))

;(Defvar is-digit (lambda (x) (

(defvar parse-number
  (lambda (input)
	 (block result
        (let ((negative (eq (car input) minus-char))
              (is-float 0)
				  (final-parse (lambda (x)
									  
									  (if is-float (* (if negative -1.0 1.0) (parse-float x))
														 (* (if negative -1 1) (parse-integer x)))))
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
							 (if (and (not is-float) (eq fst char-dot))
								  (progn
									 (set is-float t)
									 (set output (+ output fst)))
								  (if (and (sym-end fst) output)
										(return-from result (list (final-parse
																			output) input ))
										(return-from result ())))))
					 
						
					 (set input (cdr input))
					 )
			 
			 (if output (return-from result (list (final-parse output) input))
				  (return-from result ()))
			 ))))

(defvar parse-symbol
  (lambda (str) 
	 (let ((output ""))
		(println 'sym? (car str))
		(loop (and str (not (sym-end (car str))))
		 
		 (set output (+ output (car str)))
		 (println 'output output)
		 (set str (cdr str)))
		(if output
			 (list output str)
			 nil))))

(defvar parse-lisp (lambda (str) 
    (block finish
      (loop str 
				(let ((next (car str))
						(result (list)))
				  (println 'next: next)
          (if (eq next paren-start)
				  (progn
					 (set str (cdr str))
					 (set str (skip-whitespace str))
					 (loop (not (eq (car str) paren-end))
					  (let ((r (parse-lisp str)))
						 (if (not r)
							  (return-from finish "error"))
		
						 (set result (concat result (car r)))
						 (set str (skip-whitespace (cadr r)))
						 )
					  
					  
					  )
					 (set str (skip-whitespace (cdr str)))
						 
					 (return-from finish (list result str))
					 
					 )
				  )
			 
			 (let ((n (parse-number str)))
				(if n
					 (progn
						(return-from finish n))))
			 
			 (let ((n (parse-symbol str)))
				(if n
					 (return-from finish n)))
        )))))
    
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
(println "parse number: " (parse-number "-123 asd"))
(println "parse lisp: " (parse-lisp "(+ 1 2)"))
;(defvar concat (get (list 1 2) 'concat))

(concat (list 3 4) (list 3 4))
;(if  (println 'yes) (println 'no))
