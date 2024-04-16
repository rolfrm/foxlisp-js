(defvar model:square '(polygon :3d-triangle-strip (
    -1 -1 1 
    1 -1 1 
    -1 1 1 
    1 1 1)))
(defvar model:transform (mat4:identity))
(defvar model:color nil)
(defmacro model:rotation (angle x y z &rest body)
    `(let ((m (mat4:rotation (* ,angle 2 math:pi) (vec3:new ,x ,y ,z)))
           (prev-rotation model:transform))
        
       (set model:transform (mat4:multiply model:transform m))
       (progn ,@body)
       (set model:transform prev-rotation)
		 ))

(defmacro model:rotate (angle x y z &rest body)
    `(let ((m (mat4:rotation (* ,angle 2 math:pi) (vec3:new ,x ,y ,z)))
           (prev-rotation model:transform))
        
       (set model:transform (mat4:multiply model:transform m))
       (progn ,@body)
       (set model:transform prev-rotation)
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
          (setnth result k (nth item j))
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
(defun model::combine-models (models)
    (let ((result (list))
          (result-color (list))
          (any-color nil)
          )
        (for-each item models 
             ;(println 'adding item)
            (let ((model-verts (caddr (car item)))
                  (transform (cadr item))
                  (color (caddr item)))
                (when color (set any-color color))
                (unless color (set color any-color))
                  ;(println "item " item model-verts transform)
                (dotimes (i (/ (length model-verts) 3))
                    (let ((v (mat4:apply transform (vec3:from-array model-verts (* i 3)))))
                        
                        (when (and (eq i 0) (> (length result) 0))
                        ;; todo: check that the last two are not equal to the next two.
                            (push result (nth result (- (length result) 1)))
                            (push result v)
                            (when color 
                                (push result-color (nth result-color (- (length result-color) 1)))
                                (push result-color color))
                        )
                        
                        (push result v)
                        (when color 
                            (push result-color color))
                    ))
            )
        )
        (if any-color 
            (list 'polygon-strip-color (float32-array-flatten result) (float32-array-flatten result-color))
            (list 'polygon :3d-triangle-strip (float32-array-flatten result))
        )
        
        
		  )
	 )
(defmacro model:bake-keyed (key &rest model)
    `(let ((prev-transform model:transform)
           (prev-color model:color)
           (thismodel ',model)
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
                    (push baked (list model model:transform model:color))
                    ))
                  (current-drawer model:drawer)
                )
            
              (set model:drawer baker)
              (progn ,@model)
              (set model:drawer current-drawer)
              
              (set current (model::combine-models baked))
													 ;(println 'baked: current)
              (hashmap-set model::baked-models key current)
              )
            
				(set model:transform prev-transform)
        (set model:color prev-color))
        (model:draw current)
    ))

(defmacro model:bake (&rest model)
    `(let ((prev-transform model:transform)
           (prev-color model:color)
           (thismodel ',model)
			  (key ,(if (eq (car model) :key) (cadr model) (list 'quote model)))
           (current (hashmap-get model::baked-models key)))
        (unless current
            (set model:transform (mat4:identity))
            (set model:color nil)
            (let (
                (baked (list))
                (baker (lambda (model) 
                    (push baked (list model model:transform model:color))
                    ))
                  (current-drawer model:drawer)
                )
            
              (set model:drawer baker)
              (progn ,@model)
              (set model:drawer current-drawer)
              
              (set current (model::combine-models baked))
													 ;(println 'baked: current)
              (hashmap-set model::baked-models thismodel current)
              )
            
				(set model:transform prev-transform)
        (set model:color prev-color))
        (model:draw current)
    ))

(defmacro model:offset (x y z &rest body)
    `(let ((m (mat4:translation  ,x ,y ,z))
          (prev-translation model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        (progn ,@body)
        (set model:transform prev-translation)
    ))
(defmacro model:scale (x y z &rest body)
    `(let ((m (mat4:scale  ,x ,y ,z))
          (prev-model model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        (progn ,@body)
        (set model:transform prev-model)
    ))
(defmacro model:with-color (r g b &rest body)
    `(let ((prev-color model:color))
        (set model:color (vec3:new ,r ,g ,b))
        (progn ,@body)
        (set model:color prev-color)
    ))

(defmacro model:rgb (r g b &rest body)
    `(model:with-color ,r ,g ,b ,@body)
)

(defmacro model:rgb2 (lst &rest body)
    `(model:with-color (vec3:x ,lst) (vec3:y ,lst) (vec3:z ,lst) ,@body)
)


(defvar model:drawer (lambda (m) (println 'no-drawer-model: m)))

(defmacro model:with-draw (f &rest body)
   `(let ((current-drawer model:drawer))
    (set model:drawer ,f)
    (progn ,@body)
    (set model:drawer current-drawer)
   )
)

(defun model:draw (model)
    ;(println 'drawdrawdraw model)
    (model:drawer model)
)

(defun model:cube ()
    (model:bake 
    (dotimes (i 4)
        (model:rotation (* i 0.25) 1.0 0.0 0.0
            (model:draw model:square))
    )
    (dotimes (i 2)
        (model:rotation (+ (* i 0.5) 0.25) 0.0 1.0 0.0
            (model:draw model:square))
    ))
)

(defun model:red-cube ()
    (model:with-color 1.0 0.0 0.0
        (model:cube)
    )
	 )

(defun model:vertex-access (model)
  (if (eq (cadr model) :3d-triangle-strip)
		(caddr model)
		(if (eq (car model) 'polygon-strip-color)
			 (cadr model)
			 (raise "unable access vertexes") 
		)

  ))

(defun model:color-access (model)
  (if (eq (cadr model) :3d-triangle-strip)
		nil
		(if (eq (car model) 'polygon-strip-color)
			 (caddr model)
			 (raise "unable access vertexes") 
		)))


(defun model:vertex-process (model vertex-process)
  (let ((a (model:vertex-access model)))
	 (dotimes (i 0 (length a) 3)
		(let ((v (slice a i 3)))
		  (vertex-process v)
		  ))))

(defun model:process-colors(model color-process base-color)
  (let ((a (model:vertex-color-access model)))
	 (unless a
		(set model (list 'polygon-strip-color (float32-array-from (model:vertex-access model))
							  (
		)

  )))))
  

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
				  ;(println seg)
				  
				  (set vertex-list (concat (list z2 y2 x2 z1 y1 x1) vertex-list))
				  ;(println '>> vertex-list)

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
            ;(println i j)
            (when (and (eq 0 j) (> i 0))
                ;make degenerate triangle
                ;(println 'degenerate: x y z x y z2)
                (set vertices (concat vertices (list x y z x2 y2 z2)))
            )
            ;(println x y z x2 y2 z2)
        (set vertices (concat vertices (list x y z x2 y2 z2)))
        )))
      )
      (list 'polygon :3d-triangle-strip  vertices)
      
      ))

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
        (list 'polygon :3d-triangle-strip vertices)
    )
))

(defvar model::sphere12 (model::generate-sphere-2 8 8 1))
(defvar model::sphere5 (model::generate-sphere-2 5 5 1))
(defun model:sphere12 ()
    ;(model:with-color 1 1 1
    (model:bake 
        (model:draw model::sphere12))
	 )



(defun model:sphere ()
    ;(model:with-color 1 1 1
    (model:bake 
        (model:draw model::sphere12))
)
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
	  -0.5 0 -0.5)
	 ))
(defun model:pyramid ()
  (model:draw model::pyramid))

(defvar model::tile '(polygon :3d-triangle-strip (0 0 0
                                                 0 0 1
                                                 1 0 0
                                                  1 0 1)))
(defun model:tile () (model:draw model::tile))

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
  (model:offset 0.0 -0.0 -0.5
    (model:tile)))

(defun model:z-tile()
  (model:offset -0.5 -0.0 -0.0
    (model:tile)))

(defun model:tile-centered ()
  (model:offset -0.5 0.0 -0.5
    (model:tile)))

(defvar model::fronttile '(polygon :3d-triangle-strip (0 0 0
																		 1 0 0 
																		 0 1 0
																		 1 1 0)))
(defun model:fronttile ()
  (model:draw model::fronttile))


(defun gen-heightmap (l x y x2 y2 step)
  ! let ((vertexes (list))
			)
  (dotimes (i x (+ x2) step)
	 	 (let ((len (length vertexes)))
		(when (> len 0)
		  (push vertexes (nth vertexes (- len 3)))
		  (push vertexes (nth vertexes (- len 2)))
		  (push vertexes (nth vertexes (- len 1)))
	 	  (push vertexes (+ i step))
		  (push vertexes (l (+ i step) y ))
		  (push vertexes y)
		
		  )
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
  vertexes
 
)
