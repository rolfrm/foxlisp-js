(defvar make-map makemap_)
(defvar parse-integer parseInt)
(defvar parse-float parseFloat)
(defvar > _op_gt)
(defvar < _op_lt)
(defvar >= _op_gte)
(defvar <= _op_lte)
(defvar << op_leftshift)
(defvar >> op_rightshift)
(defvar logor op_or)
(defvar logand op_and)

(defvar xor op_xor)
(defvar eval eval2)
(defvar undefined __undefined)
(defvar ! not)
(setmacro lambda (_lambda (&rest code)
								  `(_lambda ,@code)))


(setmacro defmacro
			 (lambda (name args &rest code)
				`(setmacro ,name
							  (lambda ,args ,@code))))

(defvar defun::codemap (%js "new Map()"))

(defvar defun:get-code (lambda (name)
								 (defun::codemap.get name)))

(defmacro defun (name args &rest code)
  (defun::codemap.set name (list args code))
  `(defvar ,name
	  (lambda ,args ,@code)))

(defun make-error(o)
  (%js "new Error(o)"))

(defmacro raise (&rest body)
  `(_raise ,(car body)))

(defvar type-of _typeof)

(defmacro and (&rest args)
  (if (eq args.length 1)
		(car args)
		(if args.length
			 
			 `(let ((and_value_store ,(car args)))
				 (if and_value_store
					  (and ,@(cdr args))
					  and_value_store
					  ))
			 1)))

(defmacro or (&rest args)
  (if (eq args.length 1)
		(car args)
		(if args.length
			 `(let ((or_value_store ,(car args)))
				 (if or_value_store
					  or_value_store
					  (or ,@(cdr args))))
			 1)))

(defmacro block(sym &rest body)
  `(_block ,sym (progn ,@body)))

(defmacro th (loc n)
  `(%js ,loc "[" ,n "]"))

(defun object? (item) (eq (type-of item) "object"))
(defun null? (item) (and (object? item) (not item)))  
(defun list? (x) (and (eq (type-of x) "object") (Array.isArray x)))
(defun string? (x) (eq (type-of x) "string"))
(defmacro undefined? (x) `(eq (type-of ,x) "undefined"))
(defmacro bound? (x) `(not (undefined? x)))
(defun symbol? (item) (and item (eq item.type "symbol")))
(defun number? (item) (eq (type-of item) "number"))

(defun <> (a b c)
  (and (< a b) (< b c)))

(defun <=> (a b c)
  (and (<= a b) (<= b c)))

(defun sign(a)
  (if (< a 0)
		-1
		1))

(defmacro unless (test &rest actions)
  `(if ,test nil (progn ,@actions)))

(defmacro when (test &rest actions)
  `(if ,test (progn ,@actions) nil))

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
  (f.apply nil lst))

(defun funcall(f &rest args)
  (apply f args))

(defun length(list) list.length)
(defun make-object ()(%js "({})"))

(defun makehashmap ()(%js "new Map()"))

(defun make-hash-map ()(%js "new Map()"))

(defvar *types* (makehashmap))
(defun hashmap-set(map key value)
  (map.set key value))

(defun hashmap-get(map key)
  (map.get key))

(defun hash-map-set (map key value)
  (map.set key value))

(defun hash-map-get (map key)
  (map.get key))

(defun hashmap-delete(map key)
  (map.delete key))

(defun hashmap-keys(map)
  (Array.from (map.keys)))

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
	 `(progn ,@tests)))

(defun equals?(a b)
  (block return2
	 (if (list? a)
		  (if (list? b)
				(if (eq (length a) (length b))
					 (loop (length a)
					  (unless (equals? (car a) (car b))
						 (return-from return2 nil))
					  (set a (cdr a))
					  (set b (cdr b)))))
		  (eq a b))))

(defun value->string (value) (__valueToString value))
(defun string->symbol (string) (__makesym string))
(defun symbol-name (symbol) symbol.value)

(defun map (f lst)
  (let ((out (list)))
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
		 (let ((err '("assertion failed" ,condition)))
			(println 'raising err)
			(raise err  ))))

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

(defun is-whitespace (front)
  (or (eq space front)
		(eq tab front)
		(eq newline front)))

(defun skip-whitespace (str) 
  (loop (let ((front (car str))) (is-whitespace front))
   (set str (cdr str)))
  str)

(defvar **constants** (makehashmap))

(defun is-constant(sym)
  (hashmap-get **constants** sym))

(defun get-constant (sym)
  (hashmap-get **constants** sym))

(defmacro defconstant (variable value)
  `(progn
	  
	  (defvar ,variable (let ((v ,value))
								 (when (is-constant ',variable)
									(raise (list "constant already defined: " ',variable)))
								 (hashmap-set **constants** ',variable v)
								 
								 v))
	  
	  ))

(defconstant nil ())
(defvar t (eq 1 1))
(defconstant false nil)
(defconstant minus-char (car "-"))
(defconstant plus-char (car "+"))
(defconstant char-0 (car "0"))
(defconstant char-9 (car "9"))
(defconstant char-dot (car "."))
(defconstant char-newline (car "
"))

(defun sym-end (x)
  (or (is-whitespace x)
		(eq paren-start x)
		(eq paren-end x)))

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
        (if negative
				(set input (cdr input))
            (if (eq plus-char (car input))
                (set input (cdr input))
                ))
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
				  
				  (set input (cdr input)))
		  
		  (if output (return-from result (list (final-parse output) input))
				(return-from result ()))))))

(defun parse-symbol (str) 
  (let ((output ""))
	 (loop (and str (not (sym-end (car str))))
	  (set output (+ output (car str)))
	  (set str (cdr str)))
	 (if output
		  (list (makesym output) str)
		  nil)))

(defun parse-lisp (str) 
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
							(set str (skip-whitespace (cadr r)))))
						(set str (skip-whitespace (cdr str)))
						
						(return-from finish (list result str))))
				
				(let ((n (parse-number str)))
				  (when n
					 (return-from finish n)))
				
				(let ((n (parse-symbol str)))
				  (if n
						(return-from finish n)))))))

(defvar gensym::counter 0)

(defun gensym(prefix)
  (string->symbol (concat "##" (or prefix "G") (incf gensym::counter))))

(defun link-ends(lists)
  (if (> (length lists) 2)
		(concat (car lists)
				  (list (link-ends  (cdr lists))))
		(concat (car lists)
				  (list (cadr lists)))))

(defmacro case (&rest cases)
  (assert (> (length cases) 0) "Case expects more than one argument")
  (let ((value (car cases))
		  (out-cases (list)))
    (let ((cases2 (cdr cases)))
      (loop (length cases2)
	    (let ((thiscase (car cases2)))
			(assert (> (length thiscase) 0)
					  "Case must have a check and evaluation code")
         (if (eq :otherwise (car thiscase))
				 (set out-cases (concat out-cases (list `(progn ,@(cdr thiscase)))))
				 
		 		 (let ((c `(if (eq cons-value ,(car thiscase)) (progn ,@(cdr thiscase)))))
					(set out-cases (concat out-cases (list c))))))
       (set cases2 (cdr cases2))))
	 `(let ((cons-value ,value))
		 ,(link-ends out-cases))))


(defmacro foreach (sym list &rest body)
  (let ((reverse (eq sym 'reverse)))
	 (when reverse
		
	 (set sym list)
	 (set list (car body))
	 (set body (cdr body))
	 (println 'reverse sym list body)
	 )
  `(let ((for-each-lst ,list)
			(,sym nil)
			(__i ,(if reverse `(- (length for-each-lst) 1) 0))
			(__len ,(if reverse 0 `(length for-each-lst))))
	  (loop (,(if reverse '>= '<) __i __len)
		(set ,sym (nth for-each-lst __i))
		(set __i (,(if reverse `- `+) __i 1))
		,@body)
	  )))

(defmacro dotimes (sym-count &rest body)
  (let ((sym (car sym-count))
		  (l (length sym-count))
		  (start (if (<= l 2) 0 (cadr sym-count))) 
		  (count (if (<= l 2) (cadr sym-count) (caddr sym-count)))
		  (step (if (eq l 4) (nth sym-count 3) 1)))
	 
	 `(let ((,sym ,start))
		 (loop (< ,sym ,count)
		  ,@body
		  (incf ,sym ,step)))))

(defmacro dotimes! (sym-count &rest body)
  (let ((sym (car sym-count))
		  (l (length sym-count))
		  (start (if (<= l 2) 0 (cadr sym-count))) 
		  (count (if (<= l 2) (cadr sym-count) (caddr sym-count)))
		  (step (if (eq l 4) (nth sym-count 3) 1))
		  (code (list 'progn)))
	 
	 (let ((i start))
		(loop (< i count)
		 (push code `(const ((,sym ,i)) ,@body))
		 (incf i step)))
	 code))

(defmacro cond (&rest cases)
  (let ((out-cases (list)))
    (foreach item cases
				  (assert  (> (length item) 1))
				  (let ((c `(if ,(car item) (progn ,@(cdr item)))))
					 ;; append the case to the list of cases.
					 (set out-cases (concat out-cases (list c)))))
	 (link-ends out-cases)))

(defun index-of (item lst equalp)
  (if equalp
		(block iter
		  (dotimes (i (length lst))
			 (when (equals? (th lst i) item)
				(return-from iter i))))
		(lst.indexOf item)))

(defun memq (item lst)
  (skip lst (index-of item lst)))

(defvar min Math.min)
(defvar max Math.max)
(defvar abs Math.abs)

(defvar floor Math.floor)
(defvar round Math.round)
(defvar math:power Math.pow)
(defvar expt math:power)

(defun clamp (minimum v maximum)
  (min maximum (max v minimum)))

(defmacro incr (sym incr_value)
  `(set ,sym (+ ,sym ,(or incr_value 1))))

(defun push (location value)
  (location.push value))

(defun pop (location)
  (location.pop))

(defmacro swap (a b)
  `(let ((tmp ,b))
	  (set ,b ,a)
	  (set ,a tmp)))


(defmacro for (varsym start stop increment &rest body)
  `(let ((,varsym ,start))
	  (loop ,stop
		,@body
		,increment)))

(defun order-by (lst f)
  (let ((lst2 (lst.slice)))
	 (lst2.sort (lambda (a b)
					  (if (> a b) 
							1
							(if (< a b) 
								 -1 0))))
	 lst2))

(defun select (list f)
  (let ((out (list.slice))
        (l list.length))
    (for i 0 (< i l) (incf i) 
			(setnth out i (f (getnth list i))))
    out))

(defun where (lst f)
  (let ((out-lst (list)) (l (length lst)))
    (foreach x lst  
				  (when (f x)
					 (push out-lst x)))
    out-lst))

(defun aggregate (lst f)
  (when lst 
    (let ((v (car lst)))
	   (foreach x (cdr lst)
					 (set v (f v x)))
	   v)))

(defun take (lst n)
  (lst.slice 0 n))

(defun skip (lst n)
  (lst.slice n))

(defun async-call (f)
  (%js "async function (){" f "()}()"))

(defun function-signature (f)
  (concat (list f.lispname) f.lispargs))

(defvar *loaded-files* (makehashmap))
(defun load (file)
  (let ((dir (loadcontext.split "/")))
    (dir.pop)
    (dir.push file)
    (let ((newpath (dir.join "/")))
      (loadfile newpath))))

;; math
(defconstant math:pi Math.PI)
(defconstant math:2pi (* Math.PI 2))
(defconstant math:pi/2 (/ Math.PI 2))

(defvar math:sin Math.sin)
(defvar math:cos Math.cos)
(defvar math:tan Math.tan)
(defvar math:asin Math.asin)
(defvar math:acos Math.acos)
(defvar math:atan Math.atan)
(defvar math:atan2 Math.atan2)
(defvar math:sqrt Math.sqrt)
(defvar sqrt math:sqrt)

(defconstant math:sqrt2 (math:sqrt 2.0))
(defconstant math:sqrt3 (math:sqrt 3.0))

(defun math:random (min max)
  (+ (* (Math.random) (- max min)) min))

(defun lisp::make-float32-array (n)
  (%js "new Float32Array(n)"))

(defun lisp::make-int32-array (n)
  (%js "new Int32Array(n)"))

(defun float32-array-sized(size)
  (lisp::make-float32-array size))

(defun float32-array (&rest items)
  (Float32Array.from items))

(defun float32-array-from (list)
  (Float32Array.from list))

(defun float32-array-from2 (list)
  (Float32Array.from list))

(defun subarray(x i n)
  (x.subarray i (+ i n)))

(defmacro type-of(x)
  `(handle-errors
	 (_typeof ,x)
	 (e "undefined")))

(defun object? (item) (eq (type-of item) "object"))
(defun null? (item) (and (object? item) (not item)))

(defmacro fn (&rest args0)
  `(lambda ,@args0))



(defun deep-compare (a b)
  "sort order: list, string, number."
  (block return2
	 (if (eq a b)
		  0
	 (if (list? a)
		  (if (list? b)
				(if (eq (length a) (length b))
					 (dotimes (i (length a))
						(let ((sub (deep-compare (th a i) (th b i))))
						  (unless (eq sub 0)
							 (return-from return2 sub))))
					 (- (length a) (length b)))
				1)
		  (if (list? b)
				-1
				(if (string? a)
					 (if (string? b)
						  (a.localeCompare b)
						  1)
					 (if (string? b)
						  -1
						  (if (number? a)
								(if (number? b)
									 (- a b)
									 1)
								(if (number? b)
									 -1
									 0 ;;Cannot compare this kind of object!
									 )))))))))


(load "prime.lisp")


(defun string-hash (a)
  (let ((hash -53811313)
		  (l (length a)))
	 (dotimes! (_i 2)
				  (dotimes (i l)
					 (set hash (%js "((hash + a.charCodeAt(i)) * 13191721 |0)"))))
	 hash))
	 

(defun deep-hash (a)
  (if (list? a)
		(let ((l (length a))
				(s (%js `((,(prime! 1) + ,(prime! 2) * a "|" 0)))))
		  (dotimes (_i 1)
			 (dotimes (i l)
				(let ((sub (deep-hash (th a i))))
				  (dotimes (__i 2)
					 (set s (%js "(((s + sub) | 0) * 13191721 | 0)"))))))
		  s)
		(if (Number.isInteger a)
			 a
			 (if (string? a)
				  (string-hash a)
				  (if (symbol? a)
						(%js "((348347489 + (a.index * 1000001911))|0)")
						(string-hash (value->string a)))))))



(defun array-with-length (n)
  (%js "(new Array(n))"))

(defmacro new-object(&rest args)
  1
  )
(defmacro new-instance (type &rest args)
  `(%js "(new " ,type "(" ,@args "))")) 


(defun make-hash-map-equal()
  "This hash map uses deep equality and comparison."
  (list (array-with-length 4)))

(defvar hash2-tombstone (%js "{}"))
(defun hash2-insert(map0 value )
  "Returns the element if it was added otherwise nil."
  (let ((h (deep-hash value))
		  (map (th map0 0))
		  (l (length map))
		  (h2 (logand h (- l 1)))
		  (ltrig (/ l 2))
		  (i h2)
		  (tombstone-index nil)
		  )
	 (let ((idx
			  ;; do linear probing to find an empty spot.
			  (block linear-probing
				 (loop (< i l)
				  (let ((item (th map i)))
					 (when (or (not item) (equals? item value))
						(return-from linear-probing i))
					 (if (eq item hash2-tombstone i)
						(when (null? tombstone-index )
						  (set tombstone-index i))
						(decf ltrig))
					 (incr i)
				  ))
				 (set i 0)
				 (loop (< i h2)
				  (let ((item (th map i)))
					 (when (or (not item) (equals? item value))
						(return-from linear-probing i))
					 (if (eq item hash2-tombstone i)
						(when (null? tombstone-index )
						  (set tombstone-index i))
						(decf ltrig))
					 (incf i)))
				 -1)))
		
		(if (or (eq idx -1) (< ltrig 0))
		  (let ((a2 (list (array-with-length (* 2 l)))))
			 (dotimes (i l)
				(let ((obj (th map i)))
				  (unless (or (undefined? obj) (eq obj hash2-tombstone))
					 (hash2-insert a2 (th map i)))))
			 (set (th map0 0) (car a2))
			 (hash2-insert map0 value)
			 )
		  (progn
			 (if (and (not (th map idx)) (number? tombstone-index))
				  (progn
					 (set (th map tombstone-index) value)
					 value)
				  (let ((current-value (th map idx)))
					 (if (equals? current-value value)
						  (%js "null")
						  (set (th map idx) value)
						  ))
			 
		))))))
	 
(defun hash2-remove(map0 value)
  "Returns true if an element was removed otherwise nil."
  (let ((h (deep-hash value))
		  (map (th map0 0))
		  (l (length map))
		  (h2 (logand h (- l 1)))
		  (i h2))
	 (let ((idx
			  
			  (block linear-probing
				 (loop (< i l)
				  (let ((item (th map i)))
					 (when (not item)
						(return-from linear-probing -1))
					 (when (equals? item value)
						(return-from linear-probing i))
					 (incr i)
					 ))
				 
				 (set i 0)
				 (loop (< i h2)
				  (let ((item (th map i)))
					 (when (not item)
						(return-from linear-probing -1))
					 (when (equals? item value)
						(return-from linear-probing i))
				
					 (incf i)))
				 -1)))
	  
		(unless (eq idx -1)
		  (set (th map idx) hash2-tombstone)
		  t))))

(defun hash2-get(map0 value)
  "Returns true if an element was removed otherwise nil."
  (let ((h (deep-hash value))
		  (map (th map0 0))
		  (l (length map))
		  (h2 (logand h (- l 1)))
		  (i h2))
	 (block linear-probing
		(loop (< i l)
		 (let ((item (th map i)))
			(when (not item)
			  (return-from linear-probing nil))
			(when (equals? item value)
			  (return-from linear-probing item))
			(incr i)
			))
				 
		(set i 0)
		(loop (< i h2)
		 (let ((item (th map i)))
			(when (not item)
			  (return-from linear-probing -1))
			(when (equals? item value)
			  (return-from linear-probing item))
				
			(incf i)))
		nil)))
	  
