(loadfile "lisp/lisp.lisp")

(println 123)
(defun asd (a b c) (+ a b c))
(println (concat (list 1 2 3) (list 4 5 6)))
(println (asd 1 2 3))
(println (let ((a 10)) (%js "a + 1 + 2 + 3 + 4")))
;(println (case-code (cdr '(case 3 (3 b) (4 d) (5 e) (6 g)))))
(println (link-ends '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
(println "gets here")
(assert-eq 7 (case 10 (9 (+ 1 2)) (10 'test (+ 3 4))))


(println `(1 2 3 "asd" asd (3 2 1 ,(+ 1 2) ,@(list 4 5 6) 1 2 "hello
world")))


(assert-eq 1234 (when 1 1234))
(assert-eq 1234 (unless 0 1234))
(assert-not-eq 0 (when 1 1234))
(assert-not (when 0 1234))

(assert-eq "function" (type-of (lambda (x) (+ x 2))))
(assert-eq 3 (progn 1 2 3))
(assert-eq nil (println  (progn) '<<< ))
(assert-eq 0.25 (/ 4.0))
(assert-eq 2 (/ 24 4 3))
(assert-eq 1 (/ 24 4 3 2))
(assert-eq 25 (/ -100 -4))
(assert-eq 24 (* 4 3 2))
(assert-eq 3 (* 3))
(assert-eq 2 (* 1 2))
(assert-eq 6 (* 1 2 3))
(assert-eq 1 (*))
(assert-eq 0 (+))
(assert-eq 5 (+ 3;
					 2))
(assert-eq 3 (/ (* 4 3) 4))
(assert-eq 4 (/ (* 4 3) 3))
(assert-eq 3 (/ (* 3 6) (* 2 3)))
(assert-eq 2 (/ (* 4 6) (* 4 3) ))
(assert-eq 2 (/ (* 4 6) (* 12 1) ))
(assert-eq 255 #xFF)
(assert-eq 9 #o11)
(assert-eq 15 #b1111)


;; Todo: Fix this

;(defmacro test-sym(s)
;  (assert (eq s :reverse)))

;(test-sym :reverse)

(println (lambda (x y &rest args) (list x y args)))

(assert-not-equals '(1 2 3) '(0 1 2 3))
(assert-equals '(1 2 3) '(1 2 3))
(assert-not-eq '(1 2 3) '(2 3 4))

(defvar own-keys (%js "Reflect.ownKeys"))

(let  ((console-log (%js "console.log")))
  (console.log (list "123" 111 222 'asd))

  )

(let ((sum 0))
  (foreach x '(1 2 3)
				(set sum (+ x sum)))
  (assert-eq sum 6))
(let ((sum 0))
  (foreach reverse x '(1 2 3)
				(set sum (+ x sum)))
  (assert-eq sum 6))

(assert (undefined? asdasdasdasdasd))
(let ((xyz 1))
  (assert-not (undefined? xyz)))

(let ((a 1))
  (incf a 3)
  (assert-eq a 4)
  (incf a)
  (assert-eq a 5)
  (decf a)
  (assert-eq a 4)
  (decf a 3)
  (assert-eq a 1)
  )

(defun make-sphere (center radius)
  (lambda (x y z) 

  ))

(let ((sphere '(0.0 0.0 0.0 1.0))
		(ray (list 0 0 0)))
  
  
  )

(let ((tmp 123))
  (assert-eq tmp 123))

(println (type-of nil) ">" nil)
(println (type-of (list 1 2 3)))
(assert (list? (list 1 2 3)))
(assert-not (list? nil))

(defvar not-list-items (list 1 "asd" nil (lambda () 2) (make-map)))
(defvar list-items (list (list 1 2 3) (list) (list "123")))
(defvar string-items (list "" "123" "123123123123"))
(defvar not-string-items (list 1 (list 1) nil))
(defvar symbol-items (list 'a 'asd123 (quote hej)))
(defvar not-symbol-items (list 1 "asd" (list 123)))
(foreach x not-list-items (assert-not (list? x)))
(foreach x list-items (assert (list? x)))
(println (type-of (car string-items)))
(foreach x string-items
			 (assert (string? x) "not a string item!" x))
(foreach x not-string-items (assert-not (string? x)))
(foreach x symbol-items (assert (symbol? x) "not a string item!"))
(foreach x not-symbol-items (assert-not (symbol? x) "was a string item!"))

(assert (number? 5))
(assert-not (number? "5"))
(assert-not (nth (list 1) 1))
(assert-equals (cdr "123") (cdr "323"))
(assert-equals (cddr "123") (cddr "323"))
(assert-eq (cdddr "12345") "45")
(assert-eq nil (cdddr "1"))
(assert-not (cdddr (list 1)))
(assert-eq 0 (len nil))



(handle-errors (raise "x") (y (println 'error-caught)))


(println '???)
(let ((r (block asd (handle-errors (return-from asd 123) (e 333)))))
  (println r)
  (assert (eq r 123)))

(let ((r (handle-errors (block asd (raise "oh no") (return-from asd 5))
								(e (progn (assert (eq e "oh no") ) 111)))))
  (assert (eq r 111)))

(let ((x 0) (end 10000000))
  (loop (< x end)
	(incf x 1)
	))
(let ((sum 0))
  (dotimes (i 10)
    (incf sum i)
  )
  (assert-eq 45 sum)
  )

(assert-eq nil (cdr nil))
(assert-eq nil (cddr nil))
(assert-eq nil (car nil))
(assert-eq nil (cadr nil))
(println (car nil))

(defvar str0 "asd
asd
asd ")
(assert-eq 12 (length str0))

(println '> '((1 2) (w d) (4 ( 4 5))) '< )

(assert (ismacro 'defun))
(assert-not (ismacro 'defunasdasd))

(let ((n2 (order-by (list -1 5 4 3 2 1) (lambda (x) x))))
  (assert-eq -1 (getnth n2 0))
  (assert-eq 1 (getnth n2 1))
  (assert-eq 2 (getnth n2 2))
  (println n2)
)
(let ((test-list (list 1 2 3)))
   (let ((plist (select test-list (lambda (x) (- x)))))
       (assert-eq -1 (getnth plist 0))
       (assert-eq -2 (getnth plist 1))
   )
)

(let ((test (lambda (value)
        (case value
          (1 'a)
          (2 'b)
          (otherwise 'c)
  ))))
  (assert-eq 'c (test -1))
  (assert-eq 'a (test 1))
  (assert-eq 'b (test 2))
  (assert-eq 'c (test 0))
  (assert-eq 'c (test 5))
)

(assert-eq 20 (aggregate (select '(1 2 3 4) (lambda (x) (* 2 x))) (lambda (x y) (+ x y))))
(assert-equals '(4 5) (where '(1 2 3 4 5) (lambda (x) (> x 3))))
(assert-equals '(1 2 3) (take '(1 2 3 4 5) 3))
(assert-equals '(4 5) (skip '(1 2 3 4 5) 3))


(println (apply println '(1 2 3 4)))

(defun test-server ()
(let ((server (net.createServer(lambda (c)
  (println "client connected!")
     (c.write "hello!
  ")
  (c.pipe c)
))))
  (async-call (lambda () (server.listen 11111)))
  (println 'ok-server-started)
  (async-call (lambda () 
     (let ((client (net.createConnection (%js "{port: 11111}") 
       (lambda () (println "connected to server")))
  ))
        (client.on "data" (lambda(d) (println 'got-data: d) (client.end)
           (server.close)
        
        ))))
  
  )
))
(defun hello-func-def (a b) (println a b))

(defvar hello-func-2 (let ((x 0)) (lambda (a) (incf x a))))
(println (hello-func-2 2))
(println (hello-func-2 2))

(assert-eq "(1 2 3 (asd dsa))" (value->string '(1 2 3 (asd dsa))))
(assert-eq 'abc123 (string->symbol "abc123"))

(defun cond-test (x)
   (cond 
     ((string? x)
		1 "string")
     ((number? x)
		2 "number")
     (t
		3 "unknown")))
(assert-eq (cond-test "asd") "string")
(assert-eq (cond-test 123) "number")
(assert-eq (cond-test 'hej) "unknown")

(assert-equals '(3 4 5) (memq 3 '(0 1 2 3 4 5)))
(assert-equals 33 (elt (list 11 22 33 44) 2))
(assert-equals 2 (abs -2))
(assert-equals 2 (min 6 3 8 2 5 3))
(assert-equals 8 (max 6 -3 8 2 5 3))
(assert-equals 2 (clamp 2 -1 3))
(assert-equals 3 (clamp 5 -1 3))
(assert-equals -1 (clamp -2 -1 3))

(assert-eq 2 (floor 2.5))


(println (function-signature (eval 'memq)))
(println 'all-done)
(println loadcontext)

(load "load_test.lisp")

;; test hash map
(let ((hmap (makehashmap)))
   (hashmap-set hmap "123" 1)
   (hashmap-set hmap "4" 2)
   (assert-eq 1 (hashmap-get hmap "123"))
   (assert-eq 2 (hashmap-get hmap "4"))
   (hashmap-delete hmap "123")
   (assert (not (undefined? (hashmap-get hmap "4"))))
   (assert (undefined? (hashmap-get hmap "123")))
   (hashmap-set hmap "4" undefined)
   (println (hashmap-keys hmap))
)

(load "math.lisp")
(let ((m1 (mat4:translation 1 2 3))
      (m2 (mat4:translation 2 3 4)))
  (let ((m3 (mat4:multiply m1 m2)))
    (let ((r (mat4:apply m3 (vec3:new 1 1 1))))
      (assert-eq (vec3:x r) 4.0)
      (assert (eq (vec3:y r) 6.0))
      (assert (eq (vec3:z r) 8.0))
    )))

(let ((test (vec3:dot (vec3:new 1 2 3) (vec3:new 4 5 6))))
  (assert (eq test 32.0)))

(let ((r (mat4:rotation (* math:pi 0.5) (vec3:new 0 0 1))))
  
  (let ((v (vec3:new 1 0 0)))
    (let ((v2 (mat4:apply r v)))
      (println v2)
      (assert-eq-float (vec3:x v2) 0.0)
      (assert-eq-float (vec3:y v2) -1.0)
      (assert-eq-float (vec3:z v2) 0.0)
    )))

(let ((p (mat4:perspective 2.0 1.0 0.1 1000.0)))
  (mat4:print p)
  (println (mat4:apply p (vec3:new 0.5 0.5 -1000000)))
)

(load "polygon.lisp")
(println (polygon:new '(0 0 0 1 0 0 0 1 0)))
(println 'TRANSLATE: mat4:rotate)
;(println 'TRANSLATE: mat4:multiply!)
(load "model.lisp")
(model:red-cube)

(println (float32-array-flatten (list (vec3:new 1 2 3) (vec3:new 4 5 6))))


;(println (1-))

(println (model::generate-sphere-2 4 4 0.5))
(model:sphere12)

(with-prefix model: (red-cube))
  (dotimes (i 100)
  (println (math:random -50 50))
  )



;(progn ($ println) 2 3 4)

;; inlet syntax

;(with-prefix model:
;  (offset 0 0 0
;			 ($ rotation 0 0 0)
;			 ($ scale 2 2 2)
;			 (red-cube)))

(defun print-reader (code)
  (println 'compile: code)
  code
)

(println 1 2 3 ($ +) 4 ($ *) 5 6)
(assert-eq 34 ($ +) 4 ($ *) 5 ($ * 2 3))


(progn
  ($ let ((a 2) (b 3)))
  (println (+ a b)))

(let ((v2 (mat4:apply (mat4:perspective 1.5 1.0 2 1000.0) ! vec3:new 1 1 1)))
  (println v2))

(assert-eq 3 (progn (progn ! + 1 2)))

(assert-eq 12 (let ((sum 0)) (dotimes (u 3 6) (incf sum u)) sum))
(assert-eq 6 (let ((sum 0)) (dotimes (u 4) (incf sum u)) sum))
(assert-eq 20 (let ((sum 0)) (dotimes (u 0 10 2) (incf sum u)) sum))
(assert-eq 4 (math:power 2 2))
(println (slice (list 1 2 3) 1 2))
(defvar test-matrix (mat4:perspective 1.0 1.0 0.5 1000.0))


(foreach i '(1 2 3) (println i))

(let ((y 0) (x (lambda () (dotimes (i 4) (incf y i)))))
  (x)
  (println y)
  (assert (eq 6 y)))

(assert-eq 7 (op_or 1 (op_or 2 4)))
(assert-eq 4 (op_and 12 5))
(println (op_and -5 (* 0x1FFF)))
(println (model:noisef 1.5))

(dotimes (i 100)
  (println i (model:2dnoise (* i 0.1) (* i 0.1))))

(load "sdf.lisp")
(println (sphere (vec3:new 0 0 0) (vec3:new 1 1 1) 1.0))


(let ((pts (list)) (sizes (list))
		(sdf (lambda (c) (sphere c (vec3:new 0 0 0) 1.0))))
  (sdf-points pts sizes sdf (vec3:new 0 0 0) 5.0)
  (println pts))

(let ((test-items (list 1 2 3 4 5 6)))
  (set (th test-items 3) 10)
  (assert-eq (nth test-items 3) 10))

(dotimes! (i 5)
  (println i))

(let ((a (mat4:new))
		(b (mat4:new)))
  (dotimes (i 160000)
	 (set test-matrix (mat4:multiply test-matrix test-matrix))))

(let ((m (mat4:new 1 2 3 0 5 6 7 0 9 10 11 0 13 14 15 1))
		(m3 (mat4:clone m)))
  (mat4:translatei m 5 5 5)
  (mat4:translatei m 5 -5 1)
  (println '>>> m)
  (mat4:print m)

  (let ((m2 (mat4:* m3 (mat4:translation 5 5 5)
						  
						  
						  )))
	 (dotimes (i 10);100000000)
		(let ((m4 (mat4:clone m3)))
		  (mat4:translatei m4 5 5 5)
		  (mat4:dispose m4)
		  ))
	 (mat4:print m2)))


(assert (symbol? (gensym)))
(println 'gensym: (list (gensym)) (symbol? (gensym)) (gensym))
(println gensym)
(defvar m2 (mat4:identity))
(dotimes (i 10)
  (mat4:scale m2 1.0 1.0 2.0))
(let ((axis-vector (vec3:new 1 0 0)))
  (dotimes (i 12)
	 (mat4:rotate-x m2 1.0)))

(defun mat4:is-identity(m a)
  (unless a
	 (set a 0.00001))
  (let ((eqf (lambda (i v)
					(< (abs (- (th m i) v)) a))))
	 (and (eqf 0 1)
			(eqf 1 0)
			(eqf 2 0)
			(eqf 3 0)

			(eqf 4 0)
			(eqf 5 1)
			(eqf 6 0)
			(eqf 7 0)

			(eqf 8 0)
			(eqf 9 0)
			(eqf 10 1)
			(eqf 11 0)

			(eqf 12 0)
			(eqf 13 0)
			(eqf 14 0)
			(eqf 15 1))))

(let ((c (mat4:camera-look-at (vec3:new 3 2 1) (vec3:new -10 10 10) (vec3:new 1 1 0)))
		(m2 (mat4:identity))
		)
  (mat4:rotate-x m2 1.0)
  (mat4:translate m2 1 0 0)
  (mat4:scale m2 2.0 2.0 1.0)
  (mat4:rotate-y 1.0)
  (mat4:scale m2 0.5 1.0 1.5)
  (mat4:translate m2 0 4 -3)
  (mat4:rotate-z m2 -1.0)
  (mat4:print c)
  (println 'm2: )
  (mat4:print m2)
  (let ((ci (mat4:invert c))
		  (m2i (mat4:invert m2))
		  (m3 (mat4:translation 0 2 0))
		  (m3i (mat4:invert m3))
		  )
	 (let ((inverted (mat4:* m2 m2i)))
		(mat4:print inverted)
		
		(assert (not (mat4:is-identity m2i)))
		(assert (mat4:is-identity inverted))
		(assert (not (mat4:is-identity m3i)))
		(assert (not (mat4:is-identity m3)))
		(assert (mat4:is-identity (mat4:* m3 m3i)))
		(assert (mat4:is-identity (mat4:* m3i m3)))
		)))


(let ((p (mat4:perspective 1.1 1.5 0.1 1000.0))
		(cm (mat4:camera-look-at (vec3:new 1 2 3) (vec3:new 10 11 8) (vec3:new 0 1 0)))
		(cmi (mat4:invert cm))
		(combined (mat4:* p cm))
		(combined2 (mat4:* p cmi))
		(combined-invert (mat4:invert combined))
		(test-point (vec3:new 10 11 8)))
  
  ($ let ((test-point-2 (mat4:apply cmi (vec3:new 0 0 1 )))))
  (println 'test-point-2 test-point-2))

(let ((x 10))
  ;(raise "oh no!")
  )

(defun test-get-code()
  (println 'hello))

(println (defun:get-code 'test-get-code))
(defvar defun::callstack (list))
(let ((all-fcn (hashmap-keys defun::codemap)))
  (foreach f all-fcn
				(println f (defun:get-code f))
				(when t ;(eq f 'test-get-code)
				  (let ((ca (defun:get-code f)))
					 (unless (or t (eq f 'push) (eq f 'pop))
					 (eval `(defun ,f ,(car ca)
								 
								 (push defun::callstack ',f)
								 (println 'call: defun::callstack)
								 (let ((r (progn ,@(cadr ca))))
									(pop defun::callstack)
									r)

							  )))))))
(test-get-code)

													 ;(raise "oh no 2")

(println (lisp::make-float32-array 2))
(println (mat4:identity))

(println (car (list 1 2)))

(defun emit-error()
  (raise (make-error "oh no!")))

(set lisp-parser:on-error
	  (lambda (e)
		 
		 ))

(defun count (lst f)
  (let ((i 0))
	 (foreach x lst (when (f x) (incf i)))
	 i))
	
(defun count-item (lst type)
  (count lst (fn (x) (eq x type))))
  
(defun count-newlines-before-index(str index)
  (+ 1 (count-item (take str index) (nth "
" 0))))

;(emit-error)
(println 1)
(lisp-parser:on-error 1000)
(println (count-newlines-before-index "


" 3))


(println mat4:rotate)

(let ((m4 (mat4:identity))
		(v (vec3:new 1 0 0)))
  (dotimes (i 1000000)
	 (mat4:rotate m4 0.1 v)
  ))

(println mat4:rotate)
(println mat4:rotate-x)
(println mat4:translate)
(println mat4:scale)

(defvar with:test 1)

(with (with:test 2)
		(with (with:test 3)
				(assert-eq with:test 3))
		(assert-eq with:test 2))

(assert-eq with:test 1)

(println '(1 a 2 (b 3)))

(assert-eq 11 (+ 5 (% 10 7) 3))
(assert-eq 9 (* (% 10 7) 3))

(let ((random-items (list 1 2 "asd" "dsa" "csa" (list 3 2) (list 1 2) (list 5 3) (list 1 2 3 4) 6 -3 (list -2 1 3))))
  (random-items.sort deep-compare)
  (println random-items)
  )
(println (deep-compare 1 2))
(println 'hashhh: (string-hash "123aaa"))
(println 'hashhh: (string-hash "123aab"))
(println 'hashhh: (string-hash "123aac"))
(println 'hashhh: (string-hash "123aad"))

(println 'deep-hash (deep-hash (list 1 2 3)))
(println 'deep-hash (deep-hash (list 1 2 4)))
(println 'deep-hash (deep-hash (list 1 2 5)))
(println 'deep-hash (deep-hash (list 1 2 5)))
(println string-hash)

(println hash2-insert)

(let ((m (make-hash-map-equal)))
  (hash2-insert m '(1 2 3))
  (hash2-insert m '(1 2 4))
  (println m)
  (hash2-insert m '(1 2 6))
  (hash2-insert m '(1 3 2))
  (hash2-insert m '(1 2 3))
  (hash2-insert m '(2 2 3))
  (hash2-insert m "Oh ya")
  (hash2-insert m "Oh yb")
  (hash2-insert m "Oh yc")
  (hash2-insert m "Oh ya")
  (println m)
  (hash2-remove m "Oh ya")
  (hash2-remove m '(1 2 3))
  (hash2-remove m '(1 2 4))
  (println m)
  (hash2-insert m "Oh ya")
  (hash2-insert m '(1 2 4))
  (hash2-insert m 'jek)
  (println (deep-hash 'jal ) (deep-hash 'jam) (deep-hash 'jan) (deep-hash 'jar))
  (println '??? (hash2-insert m 13)
			  (hash2-insert m '(1 3))
			  (hash2-insert m '(1 3))
			  (hash2-insert m '(3 2 1))
			  (hash2-insert m '(3 2 1))
			  (hash2-insert m '(1 3))
			  (hash2-insert m 13))
  
  (println '>> (null? (hash2-insert m 13) ))
  (println '>>> (hash2-remove m 13) (hash2-remove m 13))
  (println m)
  )

(defconstant asddddd '(1))
(println asddddd)
(defun pi-asdd()
  (println (+ math:pi asddddd)))
;; todo: Fix quoted primitive types.
(println pi-asdd (+ '3.14 3))

(load "keys.lisp")

(dotimes (i -5 5)
  (println i))

(when 0
  (println (>> 10 2))
  (println (prime! 3))

  (let ((tree (balanced-deep-tree)))
	 (balanced-insert tree '(1 2)))

  (defun make-array2 (n)
	 (new-instance Array n n))

  (println make-array2)

													 ;(let ((obj (new-object :A 1 :B 2))
													 ;		(obj2 (new-instance Array 5)))
													 ; (println obj2)
													 ; )
  
  (println deep-hash))
