(defvar model:square '(polygon :3d-triangle-strip (
    -1 -1 1 
    1 -1 1 
    -1 1 1 
    1 1 1)))
(defvar model:transform (mat4:identity))
(defvar model:color nil)

(defvar model:projection (mat4:perspective 1.1 1.0 0.1 1000.0))
(defvar model:camera (mat4:identity))
(defvar model:inverse-camera (mat4:identity))

(defun model:set-camera-fps(pos y-rot)
  

  )

(defun model:camera-look-at(eye center up)
  ($ let ((f (vec3:normalize (vec3:- center eye)))
			 (s (vec3:normalize (vec3:cross f up)))
			 (t (vec3:cross s f))))
  (mat4:new (vec3:x s) (vec3:x t) (- (vec3:x f)) 0.0
				(vec3:y s) (vec3:y t) (- (vec3:y f)) 0.0
				(vec3:z s) (vec3:z t) (- (vec3:z f)) 0.0
				(vec3:x eye) (vec3:y eye) (vec3:z eye) 1.0))

(defvar model::chain-functions (makehashmap))
(foreach item '((model:rotate-x model:rotate-x-i)
					  (model:rotate-y model:rotate-y-i)
					  (model:rotate-z model:rotate-z-i)
					  (model:offset model:offset-i)
					  (model:offset-x model:offset-x-i)
					  (model:scale model:scale-i))
			 (hashmap-set model::chain-functions (car item) (cadr item)))

(defun model::gen-chain-body(body)
  (if (eq (length body) 1)
		(let ((inplace-f (hashmap-get model::chain-functions (caar body))))
		  (if inplace-f
				`((,inplace-f ,@(cdr (car body))))
				body))
		body))
  

(defmacro model:rotate (angle x y z &rest body)
  (set body (model::gen-chain-body body))
  `(let ((m (mat4:rotation (* ,angle math:2pi) (vec3:new ,x ,y ,z)))
           (prev-rotation model:transform))
        
       (set model:transform (mat4:multiply model:transform m))
       (progn ,@body)
       (set model:transform prev-rotation)
		 ))

(defmacro model:rotate-i (angle x y z &rest body)
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:rotate angle (vec3:new ,x ,y ,z))
    ,@body))


(defmacro model:rotate-x (angle &rest body)
  (set body (model::gen-chain-body body))
    `(let ((prev model:transform))
       (set model:transform (mat4:clone model:transform))
       (mat4:rotate-x model:transform (* ,angle 2 math:pi))
       ,@body
		 (mat4:dispose model:transform)
		 (set model:transform prev)))

(defmacro model:rotate-x-i (angle &rest body)
  (set body (model::gen-chain-body body))
  
  `(progn
	  (mat4:rotate-x model:transform (* ,angle 2 math:pi))
     ,@body
	  ))


(defmacro model:rotate-y (angle &rest body)
  (set body (model::gen-chain-body body))
  
  `(let ((prev model:transform))
       (set model:transform (mat4:clone model:transform))
       (mat4:rotate-y model:transform (* ,angle 2 math:pi))
		 ,@body
		 (mat4:dispose model:transform)
       (set model:transform prev)))

(defmacro model:rotate-y-i (angle &rest body)
  
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:rotate-y model:transform (* ,angle 2 math:pi))
     ,@body))

(defmacro model:rotate-z (angle &rest body)
  (set body (model::gen-chain-body body))
  `(with (model:transform (mat4:clone model:transform))
     (mat4:rotate-z model:transform (* ,angle 2 math:pi))
     ,@body
	  (mat4:dispose model:transform)))

(defmacro model:rotate-z-i (angle &rest body)
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:rotate-z model:transform (* ,angle 2 math:pi))
     ,@body
	  ))

(defun model::math::offset-x(m x)
  (mat4:multiply! m 
						1 0 0 0
						0 1 0 0
						0 0 1 0
						x 0 0 1))

(defun model::math::offset-y(m y)
  (mat4:multiply! m 
						1 0 0 0
						0 1 0 0
						0 0 1 0
						0 y 0 1))


(defun model::math::offset-z(m z)
  (mat4:multiply! m 
						1 0 0 0
						0 1 0 0
						0 0 1 0
						0 0 z 1))

(defmacro model:offset-x (x &rest body)
  (set body (model::gen-chain-body body))
  `(with ( model:transform (mat4:clone model:transform))
			(model::math::offset-x model:transform ,x)
			,@body
			(mat4:dispose model:transform)
			))

(defmacro model:offset-y (x &rest body)
  (set body (model::gen-chain-body body))
  `(with ( model:transform (mat4:clone model:transform))
			(model::math::offset-y model:transform ,x)
			,@body
			(mat4:dispose model:transform)
			))


(defmacro model:offset-z (x &rest body)
  (set body (model::gen-chain-body body))
  `(with ( model:transform (mat4:clone model:transform))
			(model::math::offset-z model:transform ,x)
			,@body
			(mat4:dispose model:transform)
			))


(defmacro model:offset-i (x y z &rest body)
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:translate model:transform ,x ,y ,z)
     ,@body))

(defmacro model:offset-x-i (x &rest body)
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:multiply! model:transform 
							1 0 0 0
							0 1 0 0
							0 0 1 0
							,x 0 0 1)
			,@body))


(defmacro model:offset (x y z &rest body)
  (cond ((and (eq x 0) (eq y 0))
			`(model:offset-z ,z  ,@body))
		  ((and (eq x 0) (eq z 0))
			`(model:offset-y ,y  ,@body))
		  ((and (eq z 0) (eq y 0))
			`(model:offset-x ,x ,@body))
		  (t
			(progn
			  (set body (model::gen-chain-body body))
			  `(with ( model:transform (mat4:clone model:transform))
	 
						(mat4:translate model:transform ,x ,y ,z)
						,@body
						(mat4:dispose model:transform)
						)))))
(defmacro model:scale (x y z &rest body)
  (set body (model::gen-chain-body body))
  `(let ((prev model:transform))
     (set model:transform (mat4:clone model:transform))
	  (mat4:scale model:transform ,x ,y ,z)
     ,@body
	  (mat4:dispose model:transform)
     (set model:transform prev)))


(defmacro model:scale-uniform(x &rest body)
  `(progn
	  (model:scale ,x ,x ,x ,@body)
	  ))

(defmacro model:scale-i (x y z &rest body)
  (set body (model::gen-chain-body body))
  `(progn
	  (mat4:scale model:transform ,x ,y ,z)
     ,@body
	  ))



(defun float32-array-flatten (v)
  (let ((size 0)
		  (k 0)
		  (result nil))
	 (dotimes (i (length v))
		(incf size (length (nth v i)))
		)
	 (set result (float32-array-sized size))

    (dotimes (i (length v))
      (let ((item (nth v i)))
        (dotimes (j (length item))
          (set (th result k) (th item j))
			 (incf k))))
    result
))

(defvar flatten::code "
(arrays) => {
     let size = 0
     for(let i = 0; i < arrays.length; i++){
       size += arrays[i].length;
     }
     let result = new Float32Array(size);
     let k = 0
     for(let i = 0; i < arrays.length; i++){
        const array = arrays[i];
        for(let j = 0; j < array.length; j++){
           result[k] = array[j]
           k += 1
        }
     }
     return result
   }

")
(set float32-array-flatten (js_eval flatten::code))

(defvar model::baked-models (makehashmap))
(defun pushvec(array vec index)
  (set index (* index 3))
  (set (th array index) (th vec 0))
  (set (th array (+ index 1)) (th vec 1))
  (set (th array (+ index 2)) (th vec 2))
  )

(defun pushvec2(dst src dst-index src-index)
  (set dst-index (* dst-index 3))
  (set src-index (* src-index 3))
  (set (th dst dst-index) (th src src-index))
  (set (th dst (+ dst-index 1)) (th src (+ 1 src-index)))
  (set (th dst (+ dst-index 2)) (th src (+ 2 src-index)))
  )


(defun model::combine-models (models)
    (let ((result nil)
          (result-color nil)
          (any-color nil)
			 (size 0)
			 (k 0)
          )
		(foreach item models
					 (unless (eq size 0)
						(set size (+ size (* 2 3)))
						)
					 (set size (+ size (length (caddr (car item)))))
					 
					 )
		(set result (float32-array-sized size))
		
		
      (foreach item models
					 
					 (if (eq (car (car item)) 'polygon-strip-color)
						  (let ((model-verts (float32-array-from2 (cadr (car item))))
								  (vert-color (float32-array-from2 (caddr (car item))))
								  (transform (cadr item))
								  (color (caddr item)))
							 (unless result-color
								(set result-color (float32-array-sized size)))
							 (mat4:applyn transform model-verts)
							 (mat4:dispose transform)
							 (set any-color '(1 1 1))
						 	 (when (> k 0)
								(pushvec2 result result k (- k 1))
								(incf k)
                        (pushvec2 result model-verts k 0)
								(incf k)
                        (when color 
                          (pushvec2 result-color result-color k (- k 2))
								  (pushvec2 result-color result-color k (- k 1))
                          ))
								
							 (dotimes (i 0 (length model-verts) 3)
								(dotimes (j 3)
								  (set (th result (+ (* 3 k) j)) (th model-verts (+ j i)))
								  (set (th result-color (+ (* 3 k) j)) (th vert-color (+ i j))))
								(incf k)))
             ;(println 'adding item)
            (let ((model-verts (float32-array-from2 (caddr (car item))))
                  (transform (cadr item))
                  (color (caddr item)))
				  
                (when color (set any-color color))
                (unless color (set color any-color))
                (mat4:applyn transform model-verts)
					 (mat4:dispose transform)
                (dotimes (i (/ (length model-verts) 3))
                    (progn
                        (when (and (eq i 0) (> k 0))
                        ;; todo: check that the last two are not equal to the next two.
                          (pushvec2 result result k (- k 1))
								  (incf k)
                          (pushvec2 result model-verts k i)
								  (incf k)
                          (when color 
                            (pushvec2 result-color result-color (- k 2) (- k 3))
                            (pushvec result-color color (- k 1)))
                          )

								(pushvec2 result model-verts k i)
								
                        (when color
								  (unless result-color
									 (set result-color (float32-array-sized size)))
                          (pushvec result-color color k))
								(incf k)
                    ))
            )
				))
		  
		  (let ((flr result)
				  (flc result-color))
			 
			 (when any-color
				(assert (eq (length flr) (length flc))))
			 (if any-color 
              (list 'polygon-strip-color flr flc)
              (list 'polygon :3d-triangle-strip flr)
				  ))))

(defmacro model:bake-keyed (key &rest model)
    `(let ((prev-transform model:transform)
           (prev-color model:color)
           (key ,key)
           (current (hashmap-get model::baked-models key)))
		 ;(println 'bake-keyed key current )
        (unless current
            (set model:transform (mat4:identity))
            (set model:color nil)
            (let (
                (baked (list))
                (baker (lambda (model) 
                    ;(println 'baking model)
                    (push baked (list model (mat4:clone model:transform) model:color))
                    ))
                  (current-drawer model:drawer)
                )
            
              (set model:drawer baker)
              (progn ,@model)
              (set model:drawer current-drawer)
              
              (set current (model::combine-models baked))
              (hashmap-set model::baked-models key current)
              )
            
				(set model:transform prev-transform)
        (set model:color prev-color))
        (model:draw current)
		  ))

(defun model::baker(baked)
  (lambda (model) 
	 (push baked (list model (mat4:clone model:transform) model:color))))

(defmacro model:bake (&rest model)
    `(let ((key ,(if (eq (car model) :key) (cadr model) (list 'quote model)))
           (current (hashmap-get model::baked-models key)))
       (unless current
			(const ((prev-transform model:transform)
					(prev-color model:color)
					(prev-drawer model:drawer)
					(baked (list)))
           (set model:transform (mat4:identity))
			  (set model:color nil)
			  (set model:drawer (model::baker baked))
			  ,@model
			  (set model:drawer prev-drawer)
			  (set current (model::combine-models baked))
			  
			  (hashmap-set model::baked-models key current)
			  
			  (mat4:dispose model:transform)
			  (set model:transform prev-transform)
			  (set model:color prev-color)))
       (model:draw current)
    ))


(defmacro model:with-color (r g b &rest body)
    `(let ((prev-color model:color))
        (set model:color (vec3:new ,r ,g ,b))
        ,@body
        (set model:color prev-color)))

(defmacro model:rgb (r g b &rest body)
    `(model:with-color ,r ,g ,b ,@body))

(defmacro model:rgb2 (lst &rest body)
  `
  (let ((++thecolor ,lst))
		  (model:rgb
			(vec3:x ++thecolor)
			(vec3:y ++thecolor)
			(vec3:z ++thecolor)
			,@body)))


(defvar model:drawer (lambda (m) (println 'no-drawer-model: m)))

(defmacro model:with-draw (f &rest body)
   `(let ((current-drawer model:drawer))
    (set model:drawer ,f)
    ,@body
    (set model:drawer current-drawer)))

(defun model:draw (model)
    (model:drawer model))

(defun model:cube ()
    (model:bake 
    (dotimes (i 4)
        (model:rotate-x (* i 0.25) 1.0
            (model:draw model:square))
    )
    (dotimes (i 2)
        (model:rotate-y (+ (* i 0.5) 0.25) 1.0
							 (model:draw model:square)))))

(defun model:red-cube ()
    (model:with-color 1.0 0.0 0.0
        (model:cube)))

(defun model:vertex-access (model)
  (if (eq (cadr model) :3d-triangle-strip)
		(caddr model)
		(if (eq (car model) 'polygon-strip-color)
			 (cadr model)
			 (raise "unable access vertexes"))))

(defun model:color-access (model)
  (if (eq (cadr model) :3d-triangle-strip)
		nil
		(if (eq (car model) 'polygon-strip-color)
			 (caddr model)
			 (raise "unable access vertexes"))))

(defun model:vertex-process (model vertex-process)
  (let ((a (model:vertex-access model)))
	 (dotimes (i 0 (length a) 3)
		(let ((v (slice a i 3)))
		  (vertex-process v)))))

(defun model:process-colors(model color-process base-color)
  (let ((a (model:vertex-color-access model)))
	 (unless a
		(set model (list 'polygon-strip-color (float32-array-from (model:vertex-access model)))))))
  
(defun model::generate-sphere (radius steps)
  (let ((vertex-list (list))
         (step-phi (/ math:pi steps))
         (step-theta (/ math:2pi steps)))
    ;; Generate vertices for triangle strip
    (dotimes (i steps) ;; vertical steps
      (dotimes (j (+ steps 1)) ;; horizontal steps
        (let ((phi1 (* i step-phi))
              (phi2 (* (+ i 1) step-phi))
              (theta (* j step-theta)))
			 (let (
               (x1 (* radius (math:sin phi1) (math:cos theta)))
               (y1 (* radius (math:cos phi1)))
               (z1 (* radius (math:sin phi1) (math:sin theta)))
               (x2 (* radius (math:sin phi2) (math:cos theta)))
               (y2 (* radius (math:cos phi2)))
               (z2 (* radius (math:sin phi2) (math:sin theta))))
				(let ((seg (list x1 y1 z1 x2 y2 z2)))
				  (set vertex-list (concat (list z2 y2 x2 z1 y1 x1) vertex-list))

				  )))
		  )
		(when (< i (- steps 1))
        (let ((phi2 (* (+ i 2) step-phi))
              (theta 0))
			 (let (
               (x2 (* radius (math:sin phi2) (math:cos theta)))
               (y2 (* radius (math:cos phi2)))
               (z2 (* radius (math:sin phi2) (math:sin theta))))
				(set vertex-list (list z2 y2 x2 z2 y2 x2) vertex-list)
		  )
		)))

	 (list 'polygon :3d-triangle-strip (reverse vertex-list))))


(defun model::generate-sphere-2 (stackCount sectorCount radius)

    (let ((vertices '())
      (lengthInv (/ 1.0 radius))
      (sectorStep (/ (* 2 math:pi) sectorCount))
      (stackStep (/ math:pi stackCount)))
  (dotimes (i (+ stackCount 1))
    
    (let 
        (
        (stackAngle (- (/ math:pi 2) (* i stackStep)))
        (stackAngle2 (- (/ math:pi 2) (* (+ i 1) stackStep)))
        ( xy (* radius (math:cos stackAngle)))
        (z (* radius (math:sin stackAngle)))
        (xy2 (* radius (math:cos stackAngle2)))
        (z2 (* radius (math:sin stackAngle2))))
        
    (dotimes (j (+ sectorCount 1))
      (let ((sectorAngle (* j sectorStep))
            (x (* xy (math:cos sectorAngle)))
            (y (* xy (math:sin sectorAngle)))
            (x2 (* xy2 (math:cos sectorAngle)))
            (y2 (* xy2 (math:sin sectorAngle))))
            
            (when (and (eq 0 j) (> i 0))
                ;make degenerate triangle
                (set vertices (concat vertices (list x y z x2 y2 z2)))
            )
        (set vertices (concat vertices (list x y z x2 y2 z2)))
        )))
      )
      (list 'polygon :3d-triangle-strip  vertices)))

(defun model:cylinder (radius height steps)
    (let ((vertices '())
          (step (/ math:2pi steps))
          (half-height (/ height 2))
          )
        (dotimes (i steps)
            (let ((theta (* i step))
                  (theta2 (* (+ i 1) step))
                  (x1 (* radius (math:cos theta)))
                  (y1 (* radius (math:sin theta)))
                  (x2 (* radius (math:cos theta2)))
                  (y2 (* radius (math:sin theta2)))
                  )
                (set vertices (concat vertices (list x1 y1 half-height x2 y2 half-height x1 y1 (- half-height) x2 y2 (- half-height))))
            )
        )
        (set vertices (concat vertices (list (nth vertices 0) (nth vertices 1) half-height (nth vertices 3) (nth vertices 4) half-height (nth vertices 0) (nth vertices 1) (- half-height) (nth vertices 3) (nth vertices 4) (- half-height)))
        (list 'polygon :3d-triangle-strip vertices))))

(defvar model::sphere12 (model::generate-sphere-2 8 8 1))
(defvar model::sphere5 (model::generate-sphere-2 5 5 1))
(defun model:sphere12 ()
  (model:bake 
   (model:draw model::sphere12)))

(defun model:sphere ()
    (model:bake 
        (model:draw model::sphere12)))
(defun model:sphere5 ()
    (model:bake 
        (model:draw model::sphere5)))
  

(defvar model::cylinder-8 (model:cylinder 1 1 8))

(defvar model::pyramid
  '(polygon :3d-triangle-strip
	 (
	  -0.5 0 -0.5
	  0 1 0
	  0.5 0 -0.5
	  0 1 0
	  0.5 0 0.5
	  0 1 0
	  -0.5 0 0.5
	  0 1 0
	  -0.5 0 -0.5
	  0 1 0)
	 ))
(defun model:pyramid ()
  (model:draw model::pyramid))

(defvar model::tile '(polygon :3d-triangle-strip (0 0 0
                                                 0 0 1
                                                 1 0 0
                                                  1 0 1)))
(defun model:tile () (model:draw model::tile))

(defvar model::upcube 'model::upcube)
(defun model:upcube ()
  ($ model:bake)
  ($ model:offset 0.0 0.5 0.0)
  ($ model:scale 0.5 0.5 0.5) 
  (model:cube))

(defun model:downcube ()
  ($ model:bake)
  ($ model:offset 0.0 -0.5 0.0)
  ($ model:scale 0.5 0.5 0.5) 
  (model:cube))

(defun model:right-tile()
  ($ model:offset 0.0 -0.0 -0.5)
  (model:tile))

(defun model:z-tile()
  ($ model:offset -0.5 -0.0 -0.0)
  (model:tile))

(defun model:y-tile()
  ($ model:bake)
  ($ model:rotate-x 0.25)
  ($ model:offset -0.5 -0.0 -0.5)
  (model:tile))

(defun model:tile-centered ()
  ($ model:offset -0.5 0.0 -0.5)
  (model:tile))

(defvar model::fronttile '(polygon :3d-triangle-strip (0 0 0
																		 1 0 0 
																		 0 1 0
																		 1 1 0)))
(defun model:fronttile ()
  (model:draw model::fronttile))


(defun gen-heightmap (l x y x2 y2 step colorf)
  ! let ((vertexes (list)) (colors nil))
  (dotimes (i x (+ x2) step)
	 (let ((len (length vertexes)))
		($ when (> len 0))
		(push vertexes (nth vertexes (- len 3)))
		(push vertexes (nth vertexes (- len 2)))
		(push vertexes (nth vertexes (- len 1)))
	 	(push vertexes (+ i step))
		(push vertexes (l (+ i step) y ))
		(push vertexes y)
		
		)

	 (dotimes (j y (+ y2 1) step)
	 	(push vertexes (+ i step))
		(push vertexes (l (+ i step) j ))
		(push vertexes j)
		(push vertexes i)
		(push vertexes (l i j))
		(push vertexes j)
		)
	 )
  (if colorf
		(progn
		  (set colors (list))
		  (dotimes (i 0 (length vertexes) 3)
			 (let ((color (colorf (nth vertexes i) (nth vertexes (+ i 1)) (nth vertexes (+ i 2)))))
				(dotimes (j 3)
				  (push colors (nth color j)))
				))
		  
		  (list 'polygon-strip-color
				  (float32-array-from vertexes)
				  (float32-array-from colors)))
		  
		
		(list 'polygon :3d-triangle-strip
				vertexes)))

(defvar model::noisemap (list))

(defun model::resample-noise ()
  (set model::noisemap (list))
  (dotimes (i 0x2000)
	 (push model::noisemap (math:random -1.0 1.0))
	 ))
(model::resample-noise)

(defun model:noisef(x)
  (let ((b (floor x))
		  (a (- x b))
		  (b2 (op_and b 0x1FFF))
		  (c (op_and (+ 1 b2) 0x1FFF))
		  (p1 (th model::noisemap b2))
		  (p2 (th model::noisemap c)))
	 
	 (+ (* p1 (- 1 a)) (* p2 a))))

(defun model:sample2d(x y)
  (th model::noisemap (op_and 0x1FFF (+ (* 659 (floor x)) (* 1069 (floor y))))))

(defmacro model::sample2d(x y)
  `(th model::noisemap (op_and 0x1FFF (+ (* 659 ,x) (* 1069 ,y)))))

(defmacro interpolate(x1 x2 mu)
  `(+ (* ,x1 (- 1 ,mu)) (* ,x2 ,mu)))

(defun cosine-interpolate(x1 x2 mu)
  (interpolate x1 x2 (* (- 1.0 (math:cos (* mu math:pi))) 0.5)))

(defmacro cinterpolate(x1 x2 mu)
  `(interpolate ,x1 ,x2 (* (- 1.0 (Math.cos (* ,mu math:pi))) 0.5)))

(defun soft-interpolate (x1 x2 mu)
  (cosine-interpolate x1 x2 mu))

(defun model:2dnoise(x y)
  (let ((bx (floor x))
		  (by (floor y))
		  (ax (- x bx))
		  ; cosine interpolation 
		  (axcos (* (- 1.0 (Math.cos (* ax math:pi))) 0.5))
		  (ay (- y by))
		  (a1 (model::sample2d bx by))
		  (a2 (model::sample2d (+ bx 1) by))
		  (a3 (model::sample2d bx (+ by 1)))
		  (a4 (model::sample2d (+ bx 1) (+ by 1)))
		  (p1 (interpolate a1 a2 axcos))
		  (p2 (interpolate a3 a4 axcos)))
	 
	 (cinterpolate p1 p2 ay)))

(defvar model::poly-cache (makehashmap))
(defvar model::shader nil)

(defun model:init-gl-draw()
  (set model::shader (shader:get-default))
  (gl.enable gl.CULL_FACE)
  (gl.cullFace gl.BACK)
  (gl.enable gl.DEPTH_TEST))

(defun model:start-gl-draw()
  (unless model::shader
	 (model:init-gl-draw))
  (shader:use model::shader)
  (gl.clearColor 0.1 0.1 0.1 1.0)
  (gl.clear gl.COLOR_BUFFER_BIT)
  (shader:set-view model::shader model:projection)
  (shader:set-camera model::shader model:inverse-camera))


(defun model:on-draw (model)
    (let ((cached (hashmap-get model::poly-cache model)))
      (unless cached
        (set cached 
				 (if (eq (car model) 'polygon-strip-color)
                 (polygon:new (nth model 1) (nth model 2))
                 (polygon:new (nth model 2))))
          
        (hashmap-set model::poly-cache model cached))
      
      (shader:set-color model::shader (vec3:x model:color) (vec3:y model:color) (vec3:z model:color) 1.0)
      (shader:set-model model::shader model:transform)
		(polygon:draw cached)
		))
(defvar gl nil)
(defvar model:webgl-canvas nil)
(defun model:initialize-gl (canvas-id)
  (set model:webgl-canvas (document.getElementById canvas-id))
  (set gl (model:webgl-canvas.getContext "webgl2"))
  (assert gl))
