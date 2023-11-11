(defvar make-map makemap_)
(defvar parse-integer parseInt)
(defvar parse-float parseFloat)
(defvar > _op_gt)
(defvar < _op_lt)
(defvar >= _op_gte)
(defvar <= _op_lte)
(defvar null? is_null)
(defvar list? is_list)


(setmacro defmacro
			 (lambda (name args &rest code)
				
				`(setmacro ,name
							  (lambda ,args ,@code))))

(defmacro defun (name args &rest code)
  `(defvar ,name
	  (lambda ,args ,@code)))

(defmacro unless (test &rest actions)
  `(if ,test (progn) (progn ,@actions)))

(defmacro when (test &rest actions)
  `(if ,test (progn ,@actions)))

(defun symbol? (item)
  (when item (eq item.type "symbol")))

(defun length(list) (get list 'length))

(defun equals?(a b)
  (block return2
	 (if (list? a)
		  (if (list? b)
				(if (eq (length a) (length b))
					 (loop (length a)
							 (unless (equals? (car a) (car b))
								(return-from return2 true))
							 (set a (cdr a))
							 (set b (cdr b)))))
		  (eq a b)
								
  )))

(defun equals?2(a b)
  (block return2
	 (eq a b))
  )


(defun assert(condition error)
  (if condition
		(progn)
		(progn
		  (raise error))
  ))

(defmacro assert-eq (a b)
  `(assert (eq ,a ,b)
			  '("assertion failed: "
				 ,a != ,b )))

(defmacro assert-not-eq(a b)
  `(assert (not (eq ,a ,b))
			  '("assertion failed:" ,a == ,b)))

(defmacro assert-equals (a b)
  `(assert (equals? ,a ,b)
			  '("assertion failed: "
				 ,a != ,b )))

(defmacro assert-not-equals (a b)
  `(assert (not (equals?  ,a ,b))
			  '("assertion failed: "
				 ,a equals ,b )))



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

(defun is-digit(x)
  (and (>= x char-0) (<= x char-9)))

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

(defvar make-sym2 (lambda (name)
  (let ((map (make-map)))
	 (put map 'jsname name)
	 (put map 'name name)
	 map)))


(defun link-ends(lists)
  (if (> (length lists) 2)
		(concat (car lists)
				  (list (link-ends  (cdr lists))))
		(concat (car lists)
				  (list (cadr lists)))))

(defmacro case (&rest cases)
  (println 'case cases)
  (assert (> (length cases) 1) "Case expects more than one argument")
  (let ((value (car cases))
		  (out-cases (list)))
    (let ((cases2 (cdr cases)))
      (loop (length cases2)
	    (let ((thiscase (car cases2)))
			(assert (eq 2 (length thiscase)) "case must have a check and evaluation code")
         (let ((c `(if (eq cons-value ,(car thiscase)) ,(cadr thiscase))))
			  (set out-cases (concat out-cases (list c)))
			  )
			)
       (set cases2 (cdr cases2))
       )
		)
	 `(let ((cons-value ,value))
		 ,(link-ends out-cases))
	 ))

(defmacro for-each (sym list &rest body)
  `(let ((for-each-lst ,list)
			(,sym nil)
			(__i 0)
			(__len (length for-each-lst)))
	  (loop (< __i __len)
		(set ,sym (nth for-each-lst __i))
		(set __i (+ __i 1))
		,@body)
	  ))

(defun map (f lst)
  (let ((out (make-map)))
	 (put out 'length (length lst))
	 (Array.from out (lambda (_, index) (f (nth lst index))))))
  
