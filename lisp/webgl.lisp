(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")




(defun get-element-by-id (item id)
    (item.getElementById id)
)

(defun get-context (item contextname)
    (item.getContext contextname)
)

(defvar webgl-canvas (get-element-by-id document "webgl-canvas"))
(defvar gl (get-context webgl-canvas "webgl"))
(println gl)
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



(defvar m4x4identity (mat4:identity))

(defvar model (mat4:translation 0.0 0.0 -19.0))
(defvar modelview (mat4:multiply (mat4:perspective 2.0 1.0 0.1 1000.0) model))
(gl.enable gl.CULL_FACE)
(gl.cullFace gl.BACK)

(defvar animate nil)
(defvar time-component 15.0)
(defun animation-loop ()
    (set time-component (+ time-component 0.01))
    (let ((shader (shader:get-default)))
        (println shader)
        (shader:use shader)
        (shader:set-color shader 0.0 1.0 1.0 1.0)
        (shader:set-model shader model)
        (shader:set-model-view shader modelview)    
    )
    ;; lets make some funky clear-color based on time:
    (gl.clearColor (math:sin time-component) (math:cos time-component) 0.0 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (polygon:draw vertices)
    (println "animation loop")
    (when animate 
        (requestAnimationFrame animation-loop)
    )
)

(animation-loop)

;(gl.drawArrays gl.TRIANGLES 0 3)