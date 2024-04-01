(defvar model:square '(polygon :3d-triangle-strip (
    -1 -1 1 
    1 -1 1 
    -1 1 1 
    1 1 1)))
(defvar model:transform (mat4:identity))
(defvar model:color (vec3:new 1.0 1.0 1.0))
(defmacro model:with-rotation (angle x y z &rest body)
    `(let ((m (mat4:rotation ,angle (vec3:new ,x ,y ,z)))
          (prev-rotation model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        ;(println 'rotation: model:transform prev-rotation)
        (progn ,@body)
        (set model:transform prev-rotation)
    ))
(defmacro model:with-offset (x y z &rest body)
    `(let ((m (mat4:translation  ,x ,y ,z))
          (prev-translation model:transform))
        
        (set model:transform (mat4:multiply model:transform m))
        (progn ,@body)
        (set model:transform prev-translation)
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
    (model:drawer model)
)

(defun model:red-cube ()
    (model:with-color 1.0 0.0 0.0
        
    (dotimes (i 4)
        (model:with-rotation (* i math:pi/2) 1.0 0.0 0.0
            (model:draw model:square))
    )
    (dotimes (i 2)
        (model:with-rotation (* (+ (* i 2) 1) math:pi/2) 0.0 1.0 0.0
            (model:draw model:square))
    )
    )
)

