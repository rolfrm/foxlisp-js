(defvar make-map makemap_)
(defvar parse-integer parseInt)
(defvar parse-float parseFloat)
(defvar > _op_gt)
(defvar < _op_lt)
(defvar >= _op_gte)
(defvar <= _op_lte)
(defvar << op_leftshift)
(defvar >> op_rightshift)
(defvar xor op_xor)
(defvar eval eval2)
(defvar undefined __undefined)
(setmacro lambda (_lambda (&rest code)
								  `(_lambda ,@code)))


(setmacro defmacro
			 (lambda (name args &rest code)
				;(declare (type string name))
				`(setmacro ,name
							  (lambda ,args ,@code))))

(defmacro defun (name args &rest code)
  `(defvar ,name
	  (lambda ,args (progn	  
	  ,@code))))

(defun object? (item) (eq (type-of item) "object"))
(defun null? (item) (and (object? item) (not item)))  


(defun list? (x) (and (eq (type-of x) "object") (Array.isArray x)))
(defun string? (x) (eq (type-of x) "string"))
(defmacro undefined? (x) `(eq (type-of ,x) "undefined"))
(defmacro bound? (x) `(not (undefined? x)))
(defun symbol? (item) (and item (eq item.type "symbol")))
(defun number? (item) (eq (type-of item) "number"))

(defmacro unless (test &rest actions)
  `(if ,test () (progn ,@actions)))

(defmacro when (test &rest actions)
  `(if ,test (progn ,@actions) ()))

(defun caar (x) (car (car x)))
(defun cadr (x) (nth x 1))
(defun caddr (x) (nth x 2))
(defun cadddr (x) (nth x 3))
(defun cadar (x) (cadr (car x)))
(defun cddr (x) (slice x 2))
(defun cdddr (x) (slice x 3))
(defun cddddr (x) (slice x 4))
(defun elt (l i) (getnth l i))

(defun apply (f lst)
  (f.apply nil lst)
)

(defun length(list) (get list 'length))
(defvar *types* (makehashmap))
(defun hashmap-set(map key value)
	(map.set key value)
)
(defun hashmap-get(map key)
  (map.get key))

(defun hashmap-delete(map key)
	(map.delete key)
)

(defun hashmap-keys(map)
	(Array.from (map.keys))
)

(defmacro deftype (name args typedeclaration)
  `(hashmap-set *types* ',name '(,args ,typedeclaration)))

(deftype string () (satisfies string?))
(deftype number () (satisfies number?))
(defmacro check-type (type value)
  (let ((typespec (hashmap-get *types* type))
		  (tests (list)))
	 (loop (len typespec)
	  (when (eq (caar typespec) 'satisfies)
		 (tests.push `(assert (,(cadar typespec) ,value)))
		 )
	  (set typespec (cdr typespec))
	  )
	 `(progn ,@tests) 
	 ))

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

(defun value->string (value) (__valueToString value))
(defun string->symbol (string) (__makesym string))



(defun map (f lst)
  (let ((out (make-map)))
	 (put out 'length (length lst))
	 (Array.from out (lambda (_, index) (f (nth lst index))))))


(defmacro +(&rest args)
  (if (len args)
		(if (eq (len args) 1)
			 (car args)
			 `(op_add ,(car args) (+ ,@(cdr args))))
		0)) 

(defmacro -(&rest args)
  (if (len args)
		(if (eq (len args) 1)
			 `(op_sub 0 ,(car args))
			 `(op_sub ,(car args) (+ ,@(cdr args))))
		(raise "invalid number of arguments: 0")))

(defmacro *(&rest args)
  
  (if (and args (len args))
		(if (eq (len args) 1)
			 (car args)
			 `(op_mul ,(car args) (* ,@(cdr args))))
		1))

(defmacro /(&rest args)
  (if args
		(if (eq (len args) 1)
			 `(op_div 1 ,(car args))
			 `(op_div ,(car args) (* ,@(cdr args))))
		(raise "Invalid number of arguments: 0")))


(defmacro incf (sym incr)
  `(set ,sym (+ ,sym ,(or incr 1))))

(defmacro decf (sym decr)
  `(set ,sym (- ,sym ,(or decr 1))))

(defmacro assert(condition)
  `(if ,condition
		(progn)
		(raise (value->string '("assertion failed" ,condition))))
		)

(defun assert-not(condition error)
  (assert (not condition) error))

(defmacro assert-eq (a b)
  `(assert (eq ,a ,b)))

(defmacro assert-eq-float (a b)
  `(assert (< (abs (- ,a ,b)) 0.000001)))

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

(defmacro assert-float-equals (a b e)
   `(assert (< (or ,e 0.0001)) (abs (- a e)) '("assertion failed: " ,a equals ,b)))


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
(defconstant nil ())
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
  
  (assert (> (length cases) 1) "Case expects more than one argument")
  (let ((value (car cases))
		  (out-cases (list)))
    (let ((cases2 (cdr cases)))
      (loop (length cases2)
	    (let ((thiscase (car cases2)))
			(assert (eq 2 (length thiscase)) "case must have a check and evaluation code")
         (if (eq :otherwise (car thiscase))
				(set out-cases (concat out-cases (list (cadr thiscase))))
			
		 	(let ((c `(if (eq cons-value ,(car thiscase)) ,(cadr thiscase))))
			  (set out-cases (concat out-cases (list c)))
			  )
			))
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

(defmacro dotimes (sym-count &rest body)
  (let ((sym (car sym-count))
		  (count (cadr sym-count)))
  `(let ((,sym 0))
	  (loop (< ,sym ,count)
		,@body
		(incr ,sym))
	  )))

(defmacro cond (&rest cases)
  (let ((out-cases (list)))
     (for-each item cases
	   (assert-eq 2 (length item))
	   (let ((c `(if ,(car item) ,(cadr item))))
	      ;; append the case to the list of cases.
	      (set out-cases (concat out-cases (list c)))
	   )
	  )
	  (link-ends out-cases)
  )
)

(defun index-of (item lst)
   (lst.indexOf item)
)

(defun memq (item lst)
   (skip lst (index-of item lst))
)

;; this lambda has 
(defmacro lambda (args &rest code)
  (let ((declarations (if (and code (car code) (eq (caar code) 'declare))
								  (let ((decl (car code)))
									 (set code (cdr code))
									 decl)
								  (list)))
		  (type-declarations (list)))
	 (let ((checks (list)))
		(when t
		(for-each decl (cdr declarations)
					 (println decl)
					 (when (eq (car decl) 'type)
						(let ((type (cadr decl))
								(rest (cddr decl)))
						  (for-each item rest
										(checks.push `(check-type ,type ,item))) 
						  
						)
					 )))

		
		`(_lambda ,args (progn ,@checks ,@code))
	 )))


(defvar min Math.min)
(defvar max Math.max)
(defvar abs Math.abs)

(defvar floor Math.floor)

(defun clamp (v minimum maximum)
    (min maximum (max v minimum)))

(defmacro incr(sym incr_value)
  `(set ,sym (+ ,sym ,(or incr_value 1)))
  )

(defmacro push (location value)
  `(let ((loc ,location))
	  (loc.push ,value))
  )
(defmacro for (varsym start stop increment &rest body)
  `(let ((,varsym ,start))
	  (loop ,stop
		,@body
		,increment
	  )
	  ))
(defun order-by (lst f)
    (let ((lst2 (lst.slice)))
	   (lst2.sort (lambda (a b)
	      (if (> a b) 
		     1
			 (if (< a b) 
			  -1 0
			 )
		  )
	   ))
	   lst2))

(defun select (list f)
   (let ((out (list.slice))
         (l list.length)
         )
     (for i 0 (< i l) (incf i) 
	     (setnth out i (f (getnth list i))))
     out
   ))

(defun where (lst f)
   (let ((out-lst (list)) (l (length lst)))
     (for-each x lst  
	   (when (f x)
	      (push out-lst x)
	   )
	 )
   out-lst
   )
)

(defun aggregate (lst f)
   (when lst 
     (let ((v (car lst)))
	   (for-each x (cdr lst)
	      (set v (f v x)))
	   v )))

(defun take (lst n)
   (lst.slice 0 n)
)

(defun skip (lst n)
   (lst.slice n))


(defun async-call (f)
   (%js "async function (){" f "()}()") 
)

(defun function-signature (f)
	(concat (list f.lispname) f.lispargs)
)

(defmacro defun (name args &rest code)
  `(defvar ,name
	  (lambda ,args (progn
	   ;(println ',name ,@(where args  (lambda (x) (not (eq x '&rest))))) 
	  
	  ,@code))))

(defvar *loaded-files* (makehashmap))
(defun load (file)
  (let ((dir (loadcontext.split "/")))
    (dir.pop)
    (dir.push file)
    (let ((newpath (dir.join "/")))
      (loadfile newpath))
  )
)

;; math
(defvar math:pi Math.PI)
(defvar math:2pi (* Math.PI 2))
(defvar math:pi/2 (/ Math.PI 2))
(defun math:sin (x) (Math.sin x))
(defun math:cos (x) (Math.cos x))
(defun math:tan (x) (Math.tan x))
(defun math:asin (x) (Math.asin x))
(defun math:acos (x) (Math.acos x))
(defun math:atan (x) (Math.atan x))
(defun math:atan2 (y x) (Math.atan2 y x))
(defun math:sqrt (x) (Math.sqrt x))

(defun float32-array (&rest items)
  (Float32Array.from items)
)

