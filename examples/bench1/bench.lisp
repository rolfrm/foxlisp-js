(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")
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

(defvar perspective (mat4:perspective 1.5 1.0 2 1000.0))

(gl.enable gl.CULL_FACE)
(gl.cullFace gl.BACK)
(gl.enable gl.DEPTH_TEST)
(defvar poly-cache (makehashmap))
(defvar shader (shader:get-default))
(defun on-draw (model)
    
    (let ((cached (hashmap-get poly-cache model)))
      (unless cached
        (set cached 
				 (if (eq (car model) 'polygon-strip-color)
                 (polygon:new (nth model 1) (nth model 2))
                 (polygon:new (nth model 2))))
          
        (hashmap-set poly-cache model cached))
      
      (shader:set-color shader (vec3:x model:color) (vec3:y model:color) (vec3:z model:color) 1.0)
      (shader:set-model shader model:transform)
		(let ((m (mat4:clone perspective)))
			 (mat4:multiplyi m perspective model:transform)
			 (shader:set-model-view shader m)
			 (mat4:dispose m))
      (polygon:draw cached)))

(defvar time 15.0)
(defun animation-loop ()
    (set time (+ time 0.002))
    (let ((shader (shader:get-default)))
        (shader:use shader)
    )

	 
    ;; lets make some funky clear-color based on time:
    (gl.clearColor 0.1 0.1 0.1 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (with-prefix model: 
    (with-draw on-draw    
      (rgb 1 0 1
			  ($ offset -0 -0 -50)
			  ($ rotate-y time)
			  ($ rotate-x time)
			  ($ rotate-z time)
			  ($ rotate-x time)
			  ($ rotate-y time)
			  ;(bake
			  (dotimes (i 50)
				 (dotimes (j 50)
					(dotimes (k2 50)
					($ offset (- i 25) (- j 25) (- k2 25))
					($ scale 0.25 0.25 0.25)
					($ rotate-x time)
					($ rotate-y time)
					($ rotate-x time)
					($ rgb (math:sin (* j 0.3))
						(math:cos (* k2 0.3))
						(+ 0.5 (* 0.5 (math:sin time)))
						)
					($ offset 0 -0.5 0)
					(upcube))))

			  ;)
			  )))
    (requestAnimationFrame animation-loop)
    
)

(animation-loop)
