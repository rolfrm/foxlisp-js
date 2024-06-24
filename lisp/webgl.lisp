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

(defvar keydown (makehashmap))
(defun key:down (key) (hashmap-get keydown key))
(defvar events-list (list))
(defun key:clear-events ()
  (set events-list (list))
  ) 

(defun key:check-event (key type)
  (block check
	 ($ for-each k events-list)
	 ($ when (eq (car k) type))
	 ($ when (eq (cadr k) key))
	 (return-from check t)
	 ))

(defun key:on-down (key) (key:check-event key 'key-down))
(defun key:on-up (key) (key:check-event key 'key-up))

(defvar events-loaded nil)
(unless events-loaded 
    (set events-loaded t)
    (window.addEventListener "keydown"
	  (lambda (evt)
		 ($ let ((k (keys:code-to-key evt.keyCode))))
		 ($ unless (eq k 'key:f12))
		 ($ unless (eq k 'key:f5))
		 (evt.preventDefault) 
       (hashmap-set keydown k t)
       (println (keys:code-to-key evt.keyCode))
		 (push events-list (list 'key-down k))
       ))
    (window.addEventListener "keyup" (lambda (evt) 
		  ($ let ((k (keys:code-to-key evt.keyCode))))
		  ($ unless (eq k 'key:f12))
		  ($ unless (eq k 'key:f5))
		  (evt.preventDefault) 
        (hashmap-set keydown k nil)
		  (push events-list (list 'key-up k))
        ))

    (webgl-canvas.addEventListener "mousedown" (lambda (&rest args) (println args)))

)


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
        (if (not cached)
            (progn
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

(defun tree ()
  (with-prefix model:
	 (offset 0.0 -3 0.0
				(rgb 1 0.8 0.4 
					  ($ scale 1 5 1)
					  (upcube))
				(rgb 0.2 0.8 0.4 
                 ($ offset 0 7.5 0)
                 ($ scale 2 2 2) 
                 (sphere12))
				)
	 ))

(defvar bullets (list))
(defun shoot-bullet (loc dir)
  (println 'shoot-pew-pew loc dir)
  (push bullets (list loc dir))
  )


(defvar animate t)
(defvar time-component 15.0)
(defvar xrot 0.0)
(defvar cam-loc (vec3:new 0 0 0))
(defun animation-loop ()
    (set time-component (+ time-component 0.002))
    (let ((shader (shader:get-default)))
        (shader:use shader)
    )
    (when (hashmap-get keydown 'key:a)
        (set xrot (- xrot 0.01))
        (println xrot)
    )
    (when (hashmap-get keydown 'key:d)
        (set xrot (+ xrot 0.01))
        (println xrot)
    )

    (let ((r (mat4:rotation xrot (vec3:new 0 1 0)))
          (ld (mat4:apply r (vec3:new 0 0 1)))
         )
        (when (key:down 'key:w)
            (set cam-loc (vec3:sub cam-loc ld)))
        (when (key:down 'key:s)
            (set cam-loc (vec3:add cam-loc ld)))
		  (when (key:on-down 'key:space)
			 (shoot-bullet cam-loc (vec3:new 0 0 1))
			 )
		  )

	 (key:clear-events)
	 

    ;; lets make some funky clear-color based on time:
    (gl.clearColor 0.1 0.1 0.1 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (with-prefix model: 
    (with-draw on-draw    
        (rgb 1 0 1 
				 (offset 0.0 -2.0 -5.0
							
				 (rotate -0.1 1 0 0
            (rotate (- xrot) 0 1 0
            
              (offset (- (vec3:x cam-loc)) (- (vec3:y cam-loc))  (- (vec3:z cam-loc))
							 (scale -500 -500 -500
									  ($ rgb 0.5 0.9 0.0)
									  (sphere12))

              (offset (vec3:x cam-loc) (vec3:y cam-loc) (vec3:z cam-loc)
                ($ rotate xrot 0 1 0)
                ($ offset 0 1 -1)
                ($ rgb 1 0.4 0.2)
                (sphere12)
					 (offset -3 0 0
					 (rotate time-component 0 0 1
								(offset 5 0 0 (sphere12))))
              )

               (rotate time-component 0 1 0
                   

                (offset 0 3 3
                      (rgb 1 0 0
                         (dotimes (i 2)
                           ($ scale 1 1 (if (eq i 0) 1 -1))
                           ($ rotate (math:sin (* 10 time-component)) 1 0 0) 
									($ offset 0 0 0.5)
									($ rgb 0 0 1) 
									($ scale 1 0.01 1)
									(upcube))
                         (scale 1.0 0.3 0.3
                           (sphere12))
                      )
                   
                   )
                
                )
					(println bullets)

					

					(rgb 1 1 1
                (bake
					 (dotimes (i 50)
						($ offset (math:random -50 50) 0.0 (math:random -50 50))
						(tree))))

                (rgb 0.2 0.7 0.2
							($ bake) 
							($ dotimes (i 100 ))
							($ rgb 1 1 1)
							($ offset (math:random -50 50) -1.2 (math:random -50 50)) 
							(upcube)
							)

                (rgb 1 1 1
                ($ bake)
                ($ offset 0 10 -200)
                (dotimes (i 100 )
						($ rgb 1 1 1)
						($ offset (math:random -200 200) (math:random -10 50) 0 )
                  (sphere12))
                (rgb 1 1 1
                     (sphere12))
                )

                (rgb 0 1 0
                   (offset 0 0 0 
                   (scale 100 1 100
                    (   tile-centered)
                    )))
                
                ))))))
    )
    (when animate 
        (requestAnimationFrame animation-loop)
    )
)

(animation-loop)
