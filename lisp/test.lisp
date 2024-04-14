(loadfile "lisp/lisp.lisp")

(println 123)
(defun asd (a b c) (+ a b c))
(println (concat (list 1 2 3) (list 4 5 6)))
(println (asd 1 2 3))
(println (let ((a 10)) (%js "a + 1 + 2 + 3 + 4")))
;(println (case-code (cdr '(case 3 (3 b) (4 d) (5 e) (6 g)))))
(println (link-ends '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
(println "gets here")
(println (case 10 (9 (+ 1 2)) (10 (+ 3 4))))

(println `(1 2 3 "asd" asd (3 2 1 ,(+ 1 2) ,@(list 4 5 6) 1 2 "hello
world")))


(assert-eq 1234 (when 1 1234))
(assert-eq 1234 (unless 0 1234))
(assert-not-eq 0 (when 1 1234))

(assert-eq "function" (type-of (lambda (x) (+ x 2))))
(assert-eq 3 (progn 1 2 3))
(assert-eq nil (progn))
(assert-eq 0.25 (/ 4.0))
(assert-eq 2 (/ 24 4 3))
(assert-eq 1 (/ 24 4 3 2))
(assert-eq 25 (/ -100 -4))
(assert-eq 24 (* 4 3 2))
(assert-eq 1 (* 1))
(assert-eq 2 (* 1 2))
(assert-eq 6 (* 1 2 3))
(assert-eq 1 (*))
(assert-eq 0 (+))

(println (lambda (x y &rest args) (list x y args)))

(assert-not-equals '(1 2 3) '(0 1 2 3))
(assert-equals '(1 2 3) '(1 2 3))
(assert-not-eq '(1 2 3) '(2 3 4))

(defvar own-keys (%js "Reflect.ownKeys"))

(let  ((console-log (%js "console.log")))
  (console.log (list "123" 111 222 'asd))

  )
(let ((sum 0))
  (for-each x '(1 2 3)
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
(for-each x not-list-items (assert-not (list? x)))
(for-each x list-items (assert (list? x)))
(println (type-of (car string-items)))
(for-each x string-items
			 (assert (string? x) "not a string item!" x))
(for-each x not-string-items (assert-not (string? x)))
(for-each x symbol-items (assert (symbol? x) "not a string item!"))
(for-each x not-symbol-items (assert-not (symbol? x) "was a string item!"))

(assert (number? 5))
(assert-not (number? "5"))
(assert-not (nth (list 1) 1))
(assert-equals (cdr "123") (cdr "323"))
(assert-equals (cddr "123") (cddr "323"))
(assert-eq (cdddr "12345") "45")
(assert-eq nil (cdddr "1"))
(assert-not (cdddr (list 1)))
(assert-eq 0 (len nil))


(defvar  test-declare (lambda (x)
										 (declare (type string x))
										 x))

(handle-errors (raise "x") (y (println 'error-caught)))

(test-declare "123")
(handle-errors (progn (test-declare 123) (raise 'failed))
					(error (println ">>" error)
							 (assert-not (eq error 'failed))))

(let ((r (block asd (handle-errors (return-from asd 123) (e 333)))))
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
(println Infinity)


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
      ((string? x) "string")
      ((number? x) "number")
      (t "unknown")
   )

)
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

(defvar code222 "

(m, v)=>{
    
    const x = m[0] * v[0] + m[4] * v[1] + m[8] * v[2]+ m[12];
    const y = m[1] * v[0] + m[5] * v[1] + m[9] * v[2]+ m[13];
    const z = m[2] * v[0] + m[6] * v[1] + m[10] * v[2]+ m[14];
    const w = m[3] * v[0] + m[7] * v[1] + m[11] * v[2] + m[15];
   if (w != 0.0){
     v[0] = x / w;
     v[1] = y / w;
     v[2] = z / w;
   }else{
     v[0] = x;
     v[1] = y;
     v[2] = z;
  
   }
   return v;
}

")
(defvar __mat4_apply2 (js_eval code222))
(set mat4:apply __mat4_apply2)

(let ((v2 (__mat4_apply2 (mat4:perspective 1.5 1.0 2 1000.0) (vec3:new 1 1 1))))
  (println v2))

(let ((v2 (mat4:apply (mat4:perspective 1.5 1.0 2 1000.0) ! vec3:new 1 1 1)))
  (println v2))

(assert-eq 3 (progn (progn ! + 1 2)))

(assert-eq 12 (let ((sum 0)) (dotimes (u 3 6) (incf sum u)) sum))
(assert-eq 6 (let ((sum 0)) (dotimes (u 4) (incf sum u)) sum))
(assert-eq 20 (let ((sum 0)) (dotimes (u 0 10 2) (incf sum u)) sum))
(assert-eq 4 (math:power 2 2))
(println (slice (list 1 2 3) 1 2))

(let ((sph (model::generate-sphere-2 8 8 1)))
  (model:vertex-process sph  (lambda (x) (println x)))
  (println sph)
  )
