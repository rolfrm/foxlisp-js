(defun vec3:new(x y z)
    (float32-array x y z)
)
(defun vec3:from-array(arr offset)
    (float32-array (nth arr offset) (nth arr (+ offset 1)) (nth arr (+ offset 2))))

(defun vec3:x(v)
    (nth v 0))

(defun vec3:y(v) 
    (nth v 1))

(defun vec3:z(v)
    (nth v 2))

(defun vec3:length(v)
    (math:sqrt (+ (* (vec3:x v) (vec3:x v)) 
                  (* (vec3:y v) (vec3:y v)) 
                  (* (vec3:z v) (vec3:z v)))))

(defun vec3:normalize(v)
    (let ((len (vec3:length v)))
        (if (< len 0.00000001)
            (vec3:new 0 0 0)
            (vec3:new (/ (vec3:x v) len) (/ (vec3:y v) len) (/ (vec3:z v) len))
        )
    )
)



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
(defun vec3:mul-scalar (v s)
    (vec3:new (* (vec3:x v) s) (* (vec3:y v) s) (* (vec3:z v) s)))

(defun vec3:dot (v1 v2)
    (+ (* (vec3:x v1) (vec3:x v2)) 
       (* (vec3:y v1) (vec3:y v2)) 
       (* (vec3:z v1) (vec3:z v2)))
)

(defun mat4:new (&rest args)
    (if (eq 0 (length args))
        (float32-array 
            0.0 0.0 0.0 0.0 
            0.0 0.0 0.0 0.0 
            0.0 0.0 0.0 0.0 
            0.0 0.0 0.0 0.0)
        (apply float32-array args)
    )
)

(defun mat4:identity()
    (mat4:new 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))

(defun mat4:get (m row col)
    (getnth m (+ row (* col 4)))
)
(defun mat4:set (m row col val)
    (setnth m (+ row (* col 4)) val)
)

(defun mat4:multiply (a b)
  (let ((result (mat4:new)))
    
    (dotimes (i 4)
      (dotimes (j 4)
        (let ((sum 0.0))
          (dotimes (k 4)
            (set sum (+ sum (* (mat4:get a i k) (mat4:get b k j)))))
          (mat4:set result i j sum))))
    result))

(defun mat4:apply (m v)
  (let ((result (vec3:new 
                 (+ (* (mat4:get m 0 0) (vec3:x v)) 
                    (* (mat4:get m 0 1) (vec3:y v)) 
                    (* (mat4:get m 0 2) (vec3:z v)) 
                    (mat4:get m 0 3))
                 (+ (* (mat4:get m 1 0) (vec3:x v)) 
                    (* (mat4:get m 1 1) (vec3:y v)) 
                    (* (mat4:get m 1 2) (vec3:z v)) 
                    (mat4:get m 1 3))
                 (+ (* (mat4:get m 2 0) (vec3:x v)) 
                    (* (mat4:get m 2 1) (vec3:y v)) 
                    (* (mat4:get m 2 2) (vec3:z v)) 
                    (mat4:get m 2 3))))
        (w (+ (* (mat4:get m 3 0) (vec3:x v)) 
              (* (mat4:get m 3 1) (vec3:y v)) 
              (* (mat4:get m 3 2) (vec3:z v)) 
              (mat4:get m 3 3))))
                
    (if (not (eq w 0.0))
        (vec3:mul-scalar result (/ 1.0 w))
        result)))


(defun mat4:translation (x y z)
    (mat4:new 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1))

(defun mat4:perspective (fov aspect near far)
  (let ((fov-rad fov)
         (f (/ 1.0 (math:tan (/ fov-rad 2.0))))
         (znear near)
         (zfar far)
         (aspect-inv (/ 1.0 aspect)))
    (mat4:new 
     (* f aspect-inv) 0 0 0
     0 f 0 0
     0 0 (/ (- (+ zfar znear)) (- znear zfar)) -1
     0 0 (/ (* -2 zfar znear) (- znear zfar)) 0)))

(defun mat4:orthographic (left right bottom top near far)
  (mat4:new 
   (/ 2.0 (- right left)) 0 0 (/ (- (+ right left)) (- right left))
   0 (/ 2.0 (- top bottom)) 0 (/ (- (+ top bottom)) (- top bottom))
   0 0 (/ -2.0 (- far near)) (/ (- (+ far near)) (- far near))
   0 0 0 1))

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

(defun mat4:scale (x y z)
  (mat4:new x 0 0 0 
            0 y 0 0 
            0 0 z 0 
            0 0 0 1))

(defun mat4:print (m)
  (let ((outstr ""))
    (dotimes (i 4)
        (dotimes (j 4)
            (set outstr (concat outstr (value->string (mat4:get m i j)) " "))
        )
        (set outstr (concat outstr newline))
        )
    (println outstr)))



(println "mat4:" (mat4:new))