(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")

;; this part must be called to initialize gl
(defun get-element-by-id (item id)
    (item.getElementById id)
)

(defun get-context (item contextname)
    (item.getContext contextname)
)
(defvar webgl-canvas (get-element-by-id document "webgl-canvas"))
;; gl must be defined!.
(defvar gl (get-context webgl-canvas "webgl"))
(assert gl)

(defvar cube-verts '(0 0 0
                    0 1 0
                    1 1 0
                    1 0 0
                    0 0 1
                    0 1 1
                    1 1 1
                    1 0 1))
 
(defvar vertices (polygon:new '(-1 -1 0 
                                1 -1 0 
                                -1 1 0 
                                1 1 0)))

(defvar square (polygon:new '(-1 -1 0 
                                1 -1 0 
                                -1 1 0 
                                1 1 0)))


(defvar perspective (mat4:perspective 2.0 1.0 0.1 1000.0))

(gl.enable gl.CULL_FACE)
(gl.cullFace gl.BACK)
(gl.enable gl.DEPTH_TEST)
(defvar poly-cache (makehashmap))
(defvar shader (shader:get-default))
(defun on-draw (model)
    (let ((cached (hashmap-get poly-cache model)))
        (if (not cached)
            (progn
            (println '----loading-model: (car model))
                (let ((poly 
                    (if (eq (car model) 'polygon-strip-color)
                        (progn
                           
                            (polygon:new (nth model 1) (nth model 2)))
                        (polygon:new (nth model 2))))
                    )
                
                    (hashmap-set poly-cache model poly)
                    (set cached poly)
                )
            )
        )
        
        (shader:set-color shader (vec3:x model:color) (vec3:y model:color) (vec3:z model:color) 1.0)
        (shader:set-model shader model:transform)
        (shader:set-model-view shader (mat4:multiply perspective model:transform))
        (polygon:draw cached)
        )  
)
(defvar animate t)
(defvar time-component 15.0)
(defun animation-loop ()
    (set time-component (+ time-component 0.01))
    (let ((shader (shader:get-default)))
        (shader:use shader)
    )
    ;; lets make some funky clear-color based on time:
    (gl.clearColor 0.1 0.1 0.1 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (model:with-draw on-draw    
        (model:with-color 1 1 1 
        (model:with-offset 0.0 0.0 -15.0

            (model:with-rotation time-component 0.5 0.5 0.5
               (model:bake 
                  (model:with-color 1 1 1
                    (model:with-offset 3.0 0.0 0.0 (model:red-cube))
                    (model:with-color 0 1 0 (model:with-offset -3.0 0.0 0.0 (model:cube)))
                    (model:with-color 1 0 0 (model:with-offset -0.0 3.0 0.0 (model:cube)))
                    (model:with-color 0 0 1 (model:with-scale 2 0.5 0.5 (model:cube)))
                )))))
    )
    ;(polygon:draw vertices)
    ;(println "animation loop")
    (when animate 
        (requestAnimationFrame animation-loop)
    )
)

(animation-loop)

;(gl.drawArrays gl.TRIANGLES 0 3)