(defmacro vec3:new(x y z)
    `(%js "[ " ,x "," ,y "," ,z "]"))
(defun vec3:from-array(arr offset)
    (%js "[arr[offset], arr[offset + 1] , arr[offset + 2]]"))

(defmacro vec3:x(v)
    `(th ,v 0))

(defmacro vec3:y(v) 
    `(th ,v 1))

(defmacro vec3:z(v)
    `(th ,v 2))

(defun vec3:length(v)
  (let ((x (vec3:x v))
		  (y (vec3:y v))
		  (z (vec3:z v)))
    (math:sqrt (+ (* x x) (* y y) (* z z)))))

(defun vec3:length-squared(v)
  (let ((x (vec3:x v))
		  (y (vec3:y v))
		  (z (vec3:z v)))
    (+ (* x x) (* y y) (* z z))))


(defun vec3:normalize(v)
    (let ((len (vec3:length v)))
        (if (< len 0.00000001)
            (vec3:new 0 0 0)
            (vec3:new (/ (vec3:x v) len)
							 (/ (vec3:y v) len)
							 (/ (vec3:z v) len)))))

(defun vec3:normalize-z(v)
  (vec3:div-scalar v (vec3:z v)))

(defmacro vec3:apply (f a b)
    `(vec3:new (,f (vec3:x ,a) (vec3:x ,b))
               (,f (vec3:y ,a) (vec3:y ,b))
               (,f (vec3:z ,a) (vec3:z ,b))))

(defun vec3:add (v1 v2)
    (vec3:apply + v1 v2))

(defun vec3:sub (v1 v2)
  (vec3:apply - v1 v2))

(defun vec3:mul (v1 v2)
  (vec3:apply * v1 v2))

(defun vec3:div (v1 v2)
    (vec3:apply / v1 v2))

(defun vec3:cross (v1 v2)
  (vec3:new (- (* (vec3:y v1) (vec3:z v2)) (* (vec3:z v1) (vec3:y v2)))
				(- (* (vec3:z v1) (vec3:x v2)) (* (vec3:x v1) (vec3:z v2)))
				(- (* (vec3:x v1) (vec3:y v2)) (* (vec3:y v1) (vec3:x v2)))))

(defvar vec3:- vec3:sub)

(defvar vec3:+ vec3:add)

(defvar vec3:* vec3:mul)

(defvar vec3:/ vec3:div)

(defun vec3:mul-scalar (v s)
    (vec3:new (* (vec3:x v) s) (* (vec3:y v) s) (* (vec3:z v) s)))

(defun vec3:div-scalar (v s)
    (vec3:new (/ (vec3:x v) s) (/ (vec3:y v) s) (/ (vec3:z v) s)))

(defun vec3:dot (v1 v2)
    (+ (* (vec3:x v1) (vec3:x v2)) 
       (* (vec3:y v1) (vec3:y v2)) 
       (* (vec3:z v1) (vec3:z v2))))

(defun mat4:new (&rest args)
  (if (eq 0 (length args))
      (float32-array-sized 16)
      (float32-array-from args)))

(defvar mat4::stack (list))

(defun mat4:dispose (m)
  (when (< (len mat4::stack) 1000)
	 (push mat4::stack m)))

(defun mat4:clone(matrix)
  (let ((r (mat4::stack.pop)))
	 (if r
		  (r.set matrix)
		  (set r (Float32Array.from matrix)))
	 r))

(defun mat4::pop-or-create()
  (if (length mat4::stack)
		(pop mat4::stack)
		(float32-array-sized 16)))

(defmacro mat4::assign(matrix &rest args)
  `(let ((m ,matrix))
	  
	  ,@(let ((lst (list))
				 (index 0))
			(foreach item args
						 (push lst `(set (th m ,index) ,item))
						 (incf index))
			(dotimes (i (- 16 index))
			  			 (push lst `(set (th m ,index) 0.0))
						 (incf index))
			lst)))

(defun mat3x4:new()
  (float32-array-sized 12))

(defun mat4:identity()
    (mat4:new 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))

(defmacro mat4:get (m row col)
    `(th ,m (+ ,row (* ,col 4))))

(defmacro mat4:set (m row col val)
  `(set (th ,m (+ ,row (* ,col 4))) ,val))


(defun mat4:multiply (ia ib)
  (let ((result  (%js "new Float32Array(16)"))
		  (a ia)
		  (b ib))
    (dotimes! (j 4)
		 (dotimes! (i 4)
		  (let ((sum 0.0))
         (dotimes! (k 4)
				(set sum (+ sum (* (mat4:get a i k) (mat4:get b k j)))))
         (mat4:set result i j sum))))
    result))

(defun mat4:multiplyi (result a b)
  (dotimes! (i 4)
	 (dotimes! (j 4)
		 (let ((sum 0.0))
			(dotimes! (k 4)
				(set sum (+ sum (* (mat4:get a i k) (mat4:get b k j)))))
			(mat4:set result i j sum))))
  0)

(defmacro mat4:* (&rest matrixes)
  (if (length (cdr matrixes))
		`(mat4:multiply ,(car matrixes) (mat4:* ,@(cdr matrixes)))
		(car matrixes)))

(defmacro mat4:multiply! (m &rest args)
  (let ((calls (list))
		  (loads (list))
		  (used-cells (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		  (largname (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	 (dotimes (j 4)
	   (dotimes (i 4)
		  (let ((arg0 (th args (+ i (* j 4)))))
			 (setnth largname (+ i (* j 4))
						(if (list? (nth args (+ i (* j 4))))
							 (let ((name (string->symbol (concat "v" (+ i (* j 4))))))
								(push loads (list name (nth args (+ i (* j 4))) ))
								name)
							 (nth args (+ i (* j 4))))))))
	 (dotimes (j 4)
	   (dotimes (i 4)				  
		  (let ((karg (list)))
			 (dotimes (k 4)
				(let ((arg (nth largname (+ k (* j 4)))))
				  (unless (eq arg 0)
					 (let ((expr `(* ,(string->symbol (concat "m" (+ i (* 4 k)))) ,arg)))
						(when (eq arg 1)
						  (set expr.unity 1))
						(set expr.reads (+ i (* 4 k)))
						(push karg expr)))))
			 (unless  (and (eq (length karg) 1)
								(let ((expr (car karg)))
								  (and expr.unity (eq expr.reads (+ i (* j 4))))))
				(foreach x karg
							 (setnth used-cells x.reads 1))
				(push calls `(set (th ,m ,(+ i (* j 4))) (+ ,@karg)))))))
	 (dotimes (i 16)
		(when (nth used-cells i)
		  (push loads `(,(string->symbol (concat "m" i)) (th ,m ,i)))))
	 
	 `(const (,@loads)
				,@calls
				0)))



(defun mat4:apply (m v in-place)
  (let ((w (or (+ (* (mat4:get m 3 0) (vec3:x v)) 
						(* (mat4:get m 3 1) (vec3:y v)) 
						(* (mat4:get m 3 2) (vec3:z v)) 
						(mat4:get m 3 3)) 1.0))
		  (x 
         (/ (+ (* (mat4:get m 0 0) (vec3:x v)) 
					(* (mat4:get m 0 1) (vec3:y v)) 
               (* (mat4:get m 0 2) (vec3:z v)) 
               (mat4:get m 0 3)) w))
		  (y
         (/ (+ (* (mat4:get m 1 0) (vec3:x v)) 
               (* (mat4:get m 1 1) (vec3:y v)) 
               (* (mat4:get m 1 2) (vec3:z v)) 
               (mat4:get m 1 3)) w))
		  (z
         (/ (+ (* (mat4:get m 2 0) (vec3:x v)) 
               (* (mat4:get m 2 1) (vec3:y v)) 
               (* (mat4:get m 2 2) (vec3:z v)) 
               (mat4:get m 2 3)) w))
        )
	 
	 (if in-place
		  (progn
			 (setnth v 0 x)
			 (setnth v 1 y)
			 (setnth v 2 z))
		  (vec3:new x y z))))

;; todo: w is generally always 1 for affine transformations. 
;; testing shows that in some cases w is different from 1.0.
(defvar code::mat4:applyn "(m, v)=>{
    const verts = v.length;
    for(let i = 0; i < verts; i += 3){
    const x = m[0] * v[i] + m[4] * v[i+1] + m[8] * v[i+2]+ m[12];
    const y = m[1] * v[i] + m[5] * v[i+1] + m[9] * v[i+2]+ m[13];
    const z = m[2] * v[i] + m[6] * v[i+1] + m[10] * v[i+2]+ m[14];
    const w = m[3] * v[i] + m[7] * v[i+1] + m[11] * v[i+2] + m[15];
   if (w != 0.0){
     v[i+0] = x / w;
     v[i+1] = y / w;
     v[i+2] = z / w;
   }else{
     v[i+0] = x;
     v[i+1] = y;
     v[i+2] = z;
   }}}
")

(defvar mat4:applyn (js_eval code::mat4:applyn))

(defun mat4:translation (x y z)
  (mat4:new 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1))

(defun mat4:translatei (m x y z)
  (set (th m 12) (+ (* (th m 0) x)
						  (* (th m 4) y)
						  (* (th m 8) z)
						  (th m 12)))
  (set (th m 13) (+ (* (th m 1) x)
						  (* (th m 5) y)
						  (* (th m 9) z)
						  (th m 13)))
  (set (th m 14) (+ (* (th m 2) x)
						  (* (th m 6) y)
						  (* (th m 10) z)
						  (th m 14 ))))

(defun mat4:perspective (fov aspect near far)
  (let ((fov-rad fov)
         (f (/ 1.0 (math:tan (/ fov-rad 2.0))))
         (znear near)
         (zfar far)
         (aspect-inv (/ 1.0 aspect)))
    (mat4:new 
     (* f aspect-inv) 0 0 0
     0 f 0 0
     0 0 (/ (- (+ zfar znear)) (- zfar znear)) -1
     0 0 (/ (* -2 zfar znear) (- zfar znear)) 0)))

(defun mat4:orthographic (left right bottom top near far)
  (mat4:new 
   (/ 2.0 (- right left)) 0 0 (/ (- (+ right left)) (- right left))
   0 (/ 2.0 (- top bottom)) 0 (/ (- (+ top bottom)) (- top bottom))
   0 0 (/ -2.0 (- far near)) (/ (- (+ far near)) (- far near))
   0 0 0 1))

(defun mat4:camera-look-at(eye center up)
  ($ let ((f (vec3:normalize (vec3:- center eye)))
			 (s (vec3:normalize (vec3:cross f up)))
			 (t (vec3:cross s f))
			 (m (mat4:new (vec3:x s) (vec3:x t) (- (vec3:x f)) 0.0
							  (vec3:y s) (vec3:y t) (- (vec3:y f)) 0.0
							  (vec3:z s) (vec3:z t) (- (vec3:z f)) 0.0
							  0 0 0 1))))
  (mat4:translate m (- (vec3:x eye)) (- (vec3:y eye)) (- (vec3:z eye)))
  m)
				
(defun mat4:transpose-mat3 (m)
  (swap (th m 1) (th m 4))
  (swap (th m 2) (th m 8))
  (swap (th m 3) (th m 12))
  (swap (th m 6) (th m 9))
  )

(defun mat4:camera-inverse2 (m)
  (println 'invert-m)
  (mat4:print m)
  (mat4:transpose-mat3 m)
  ($ let ((t2 (vec3:new (th m 12) (th m 13) (th m 14)))
			 (t3 (mat4:apply m t2))))
  (println t2)
  (mat4:print m)
  )

(defun mat4:invert(m)
  ($ let ((s0 (- (* (th m 0) (th m 5)) (* (th m 4) (th m 1))))
			 (s1 (- (* (th m 0) (th m 6)) (* (th m 4) (th m 2))))
			 (s2 (- (* (th m 0) (th m 7)) (* (th m 4) (th m 3))))
			 (s3 (- (* (th m 1) (th m 6)) (* (th m 5) (th m 2))))
			 (s4 (- (* (th m 1) (th m 7)) (* (th m 5) (th m 3))))
			 (s5 (- (* (th m 2) (th m 7)) (* (th m 6) (th m 3))))

			 (c0 (- (* (th m  8) (th m 13)) (* (th m 12) (th m  9))))
			 (c1 (- (* (th m  8) (th m 14)) (* (th m 12) (th m 10))))
			 (c2 (- (* (th m  8) (th m 15)) (* (th m 12) (th m 11))))
			 (c3 (- (* (th m  9) (th m 14)) (* (th m 13) (th m 10))))
			 (c4 (- (* (th m  9) (th m 15)) (* (th m 13) (th m 11))))
			 (c5 (- (* (th m 10) (th m 15)) (* (th m 14) (th m 11))))

			 (idet (/ 1.0 (+ (* s0 c5)
								  (* -1 s1 c4)
								  (* s2 c3)
								  (* s3 c2)
								  (* -1 s4 c1)
								  (* s5 c0))))))
  (mat4:new
	(* idet (+ (*    (th m  5) c5) (* -1 (th m 6 ) c4) (*    (th m  7) c3)))
	(* idet (+ (* -1 (th m  1) c5) (*    (th m 2 ) c4) (* -1 (th m  3) c3)))
	(* idet (+ (*    (th m 13) s5) (* -1 (th m 14) s4) (*    (th m 15) s3)))
	(* idet (+ (* -1 (th m  9) s5) (*    (th m 10) s4) (* -1 (th m 11) s3)))
	
	(* idet (+ (* -1 (th m  4) c5) (*    (th m  6) c2) (* -1 (th m  7) c1)))
	(* idet (+ (*    (th m  0) c5) (* -1 (th m  2) c2) (*    (th m  3) c1)))
	(* idet (+ (* -1 (th m 12) s5) (*    (th m 14) s2) (* -1 (th m 15) s1)))
	(* idet (+ (*    (th m  8) s5) (* -1 (th m 10) s2) (*    (th m 11) s1)))
	
	(* idet (+ (*    (th m  4) c4) (* -1 (th m  5) c2) (*    (th m  7) c0)))
	(* idet (+ (* -1 (th m  0) c4) (*    (th m  1) c2) (* -1 (th m  3) c0)))
	(* idet (+ (*    (th m 12) s4) (* -1 (th m 13) s2) (*    (th m 15) s0)))
	(* idet (+ (* -1 (th m  8) s4) (*    (th m  9) s2) (* -1 (th m 11) s0)))
	
	(* idet (+ (* -1 (th m  4) c3) (*    (th m  5) c1) (* -1 (th m  6) c0)))
	(* idet (+ (*    (th m  0) c3) (* -1 (th m  1) c1) (*    (th m  2) c0)))
	(* idet (+ (* -1 (th m 12) s3) (*    (th m 13) s1) (* -1 (th m 14) s0)))
	(* idet (+ (*    (th m  8) s3) (* -1 (th m  9) s1) (*    (th m 10) s0)))))


(defun mat4:rotation (angle axis-vector)
  (set axis-vector (vec3:normalize axis-vector))
  (let ((rad angle) 
         (cosA (math:cos rad))
         (sinA (math:sin rad))
         (invCosA (- 1 cosA)))
    (mat4:new 
     (+ cosA (* (vec3:x axis-vector) (vec3:x axis-vector) invCosA)) (- (* (vec3:x axis-vector) (vec3:y axis-vector) invCosA) (* (vec3:z axis-vector) sinA)) (+ (* (vec3:x axis-vector) (vec3:z axis-vector) invCosA) (* (vec3:y axis-vector) sinA)) 0
     (+ (* (vec3:y axis-vector) (vec3:x axis-vector) invCosA) (* (vec3:z axis-vector) sinA)) (+ cosA (* (vec3:y axis-vector) (vec3:y axis-vector) invCosA)) (- (* (vec3:y axis-vector) (vec3:z axis-vector) invCosA) (* (vec3:x axis-vector) sinA)) 0
     (- (* (vec3:z axis-vector) (vec3:x axis-vector) invCosA) (* (vec3:y axis-vector) sinA)) (+ (* (vec3:z axis-vector) (vec3:y axis-vector) invCosA) (* (vec3:x axis-vector) sinA)) (+ cosA (* (vec3:z axis-vector) (vec3:z axis-vector) invCosA)) 0
     0 0 0 1)))

(defun mat4:rotate (m angle axis-vector)
  (const ((l (vec3:length-squared axis-vector)))
	 (if (not (eq l 1.0))
		  (set axis-vector (vec3:div-scalar (math:sqrt l)))))
  
  (const ((rad angle) 
			 (cosA (math:cos rad))
			 (sinA (math:sin rad))
			 (icosA (- 1 cosA))
			 (x (vec3:x axis-vector))
			 (y (vec3:y axis-vector))
			 (z (vec3:z axis-vector)))
	 (mat4:multiply! m
						  (+ cosA (* x x icosA))
						  (- (* x y icosA) (* z sinA))
						  (+ (* x z icosA) (* y sinA))
						  0
						
						  (+ (* x y icosA) (* z sinA))
						  (+ cosA (* y y icosA))
						  (- (* y z icosA) (* x sinA))
						  0
						  
						  (- (* x z icosA) (* y sinA))
						  (+ (* y z icosA) (* x sinA))
						  (+ cosA (* z z icosA))
						  0
						  0 0 0 1)))



(defun mat4:rotate-x (m rad)
  (const ((cosA (math:cos rad))
			 (sinA (math:sin rad))
			 (isinA (- 0 sinA)))
	 (mat4:multiply! m 
						  1 0 0 0
						  0 cosA sinA 0
						  0 isinA cosA 0
						  0 0 0 1)))

(defun mat4:rotate-y (m rad)
  (const ((cosA (math:cos rad))
			 (sinA (math:sin rad)))
	  (mat4:multiply! m 
						  cosA 0 sinA 0
						  0 1 0 0
						  (- 0 sinA) 0 cosA 0
						  0 0 0 1)))

(defun mat4:rotate-z (m rad)
  (let ((cosA (math:cos rad))
        (sinA (math:sin rad)))
	 (mat4:multiply! m 
						  cosA sinA 0 0
						  (- 0 sinA) cosA 0 0
						  0 0 1 0
						  0 0 0 1)))

(defun mat4:translate (m x y z)
  (mat4:multiply! m
     1 0 0 0
     0 1 0 0
     0 0 1 0
     x y z 1))

(defun mat4:scaling (x y z)
  (mat4:new x 0 0 0 
            0 y 0 0 
            0 0 z 0 
            0 0 0 1))

(defun mat4:scale (m x y z)
  (mat4:multiply! m
						x 0 0 0 
						0 y 0 0 
						0 0 z 0 
						0 0 0 1))

(defun mat4:print (m)
  (let ((outstr ""))
    (dotimes (i 4)
        (dotimes (j 4)
            (set outstr (concat outstr (value->string (mat4:get m i j)) " ")))
        (set outstr (concat outstr newline)))
    (println outstr)))
