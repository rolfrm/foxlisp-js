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
          (result-color (list)))
        (for-each item models 
            (let ((model-verts (caddr (car item)))
                  (transform (cadr item))
                  (color (caddr item)))
                  ;(println "item " item model-verts transform)
                (dotimes (i (/ (length model-verts) 3))
                    (let ((v (mat4:apply transform (vec3:from-array model-verts (* i 3)))))
                        
                        (when (and (eq i 0) (> (length result) 0))
                        ;; todo: check that the last two are not equal to the next two.
                            (push result (nth result (- (length result) 1)))
                            (push result v)

                            (push result-color (nth result-color (- (length result-color) 1)))
                            (push result-color color)
                        )
                        
                        (push result v)
                        (push result-color color)
                    ))
            )
        )
        (list 'polygon-strip-color (float32-array-flatten result) (float32-array-flatten result-color))
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

