(defvar model:square '(polygon :3d-triangle-strip (
    -1 -1 1 
    1 -1 1 
    -1 1 1 
    1 1 1)))
(defvar model:transform (mat4:identity))
(defvar model:color nil)
(defmacro model:with-rotation (angle x y z &rest body)
    `(let ((m (mat4:rotation ,angle (vec3:new ,x ,y ,z)))
          (prev-rotation model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        ;(println 'rotation: model:transform prev-rotation)
        (progn ,@body)
        (set model:transform prev-rotation)
    ))

(defun float32-array-flatten (v)
    (let ((result (list)))
        (dotimes (i (length v))
            (let ((item (nth v i)))
                (dotimes (j (length item))
                    (push result (nth item j))
                )
            )
        )
        
        (apply float32-array result)
))

(defvar model::baked-models (makehashmap))
(defun model::combine-models (models)
    (let ((result (list))
          (result-color (list))
          (any-color nil)
          )
        (for-each item models 
             (println 'adding item)
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
(defmacro model:bake (&rest model)
    `(let ((prev-transform model:transform)
           (prev-color model:color)
           (thismodel ',model)
           (current (hashmap-get model::baked-models thismodel)))
        (unless current
            (set model:transform (mat4:identity))
            (set model:color nil)
            (let (
                (baked (list))
                (baker (lambda (model) 
                    (println 'baking model)
                    (push baked (list model model:transform model:color))
                    ))
                  (current-drawer model:drawer)
                )
            
                (set model:drawer baker)
                (progn ,@model)
                (set model:drawer current-drawer)
                
                (set current (model::combine-models baked))
                (println 'baked: current)
                (hashmap-set model::baked-models thismodel current)
            
            )
            
        (set model:transform prev-transform)
        (set model:color prev-color))
        (model:draw current)
    ))

(defmacro model:with-offset (x y z &rest body)
    `(let ((m (mat4:translation  ,x ,y ,z))
          (prev-translation model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        (progn ,@body)
        (set model:transform prev-translation)
    ))
(defmacro model:with-scale (x y z &rest body)
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
    (dotimes (i 4)
        (model:with-rotation (* i math:pi/2) 1.0 0.0 0.0
            (model:draw model:square))
    )
    (dotimes (i 2)
        (model:with-rotation (* (+ (* i 2) 1) math:pi/2) 0.0 1.0 0.0
            (model:draw model:square))
    )

)

(defun model:red-cube ()
    (model:with-color 1.0 0.0 0.0
        (model:cube)
    )
)

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
				  (println seg)
				  
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
            (println i j)
            (when (and (eq 0 j) (> i 0))
                ;make degenerate triangle
                (println 'degenerate: x y z x y z2)
                (set vertices (concat vertices (list x y z x2 y2 z2)))
            )
            (println x y z x2 y2 z2)
        (set vertices (concat vertices (list x y z x2 y2 z2)))
        )))
      )
      (list 'polygon :3d-triangle-strip  vertices)
      
      ))

(defvar model::sphere12 (model::generate-sphere-2 8 8 1.0))
(defun model:sphere12 ()
    ;(model:with-color 1 1 1
    (model:bake 
        (model:draw model::sphere12))
)