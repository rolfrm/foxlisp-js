(defvar make-map makemap_)
(defvar parse-integer parseInt)
(defvar parse-float parseFloat)
(defvar > _op_gt)
(defvar < _op_lt)
(defvar >= _op_gte)
(defvar <= _op_lte)
(defvar null? is_null)
(defvar defun-code
	 (lambda (code)
		`(defvar ,(car code)
				(lambda ,(cadr code) ,@(cddr code)))))

(defmacro defun defun-code)

(defun assert(condition error)
  (if (not condition)
		(raise error)
  ))


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

(defvar is-digit
  (lambda (x) (and (>= x char-0) (<= x char-9))))

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
						(if (is-digit fst)
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
		(loop (and str (not (sym-end (car str))))
		 (set output (+ output (car str)))
		 (set str (cdr str)))
		(if output
			 (list (makesym output) str)
			 nil))))

(defvar parse-lisp
  (lambda (str) 
	 (block finish
      (loop str 
				(let ((next (car str))
						(result (list)))
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

(defvar make-sym2 (lambda (name)
  (let ((map (make-map)))
	 (put map 'jsname name)
	 (put map 'name name)
	 map)))

(concat (list 3 4) (list 3 4))
(println (makesym "+"))

(defvar length (lambda (list) (get list 'length)))
													 ;(if  (println 'yes) (println 'no))

(defun link-ends(lists)
  (if (> (length lists) 2)
		(concat (car lists)
				  (list (link-ends  (cdr lists))))
		(concat (car lists)
				  (list (cadr lists)))))

(defvar case-code
  (lambda (cases)
	 (assert (> (length cases) 0) "Expected more than one argument")
		(let ((value (car cases))
				(out-cases (list)))
        (let ((cases2 (cdr cases)))
          (loop (length cases2)
			  
           (let ((thiscase (car cases2)))
				 
             (assert (eq 2 (length thiscase)) "case must have a check and evaluation code")
             (let ((c (list 'if (list 'eq 'cons-value (car thiscase)) (cadr thiscase))))
					(set out-cases (concat out-cases (list c)))
					)
				 )
           (set cases2 (cdr cases2))
           )
			 )
		  `(let ((cons-value ,value))
			  ,(link-ends out-cases))
		  )))
(setmacro case case-code)




;(println (get "asd123"))													 ;(raise "assertion failed")
