(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")
(load "sdf.lisp")
;; todo: Implement a camera transform
(defun get-time ()
  (%js "Date.now()"))

;; this part must be called to initialize gl
(model:initialize-gl "webgl-canvas")
(key:load-events model:webgl-canvas)

(defvar model:projection (mat4:perspective 1.6 1.0 0.1 1000.0))

(defvar fps-display (document.getElementById "fps-display"))
(defvar blit-canvas (document.getElementById "blit-canvas"))
(defvar blit-ctx (blit-canvas.getContext "2d"))
(defvar events-loaded 0)
(defvar mouse-evt nil)
(defvar target-x 0)
(defvar target-y 0)
(defvar target-xn 0)
(defvar target-yn 0)
(defvar rect-rect 0)
(unless events-loaded
  (set events-loaded 1)
  (blit-canvas.addEventListener
	"mousemove"
	(lambda (evt)
	  (let ((item (blit-canvas.getBoundingClientRect)))
		 (set target-x (- evt.clientX item.left))
		 (set target-y (- evt.clientY item.top))
		 (set target-xn (- (/ target-x item.width 0.5) 1.0))
		 (set target-yn (- (- (/ target-y item.height 0.5) 1.0)))
		 (set rect-rect item)
		 )
	  ;(println target-x target-y target-xn target-yn)
	  (set mouse-evt evt)
	  
	  )))

(defvar time (/ (get-time) 1000.0))
(defvar last-time time)
(defun update-time()
  (set time (/ (get-time) 1000)))

(defvar item-lookup (makehashmap))

(defvar occlusion-lookup (makehashmap))
(defvar visible-keys (makehashmap))
(defun occlusion-query::start(key)
  (let ((existing-query (hashmap-get occlusion-lookup key))
		  (model:visible nil))
	 (unless existing-query
		(set existing-query (gl.createQuery))
		(hashmap-set occlusion-lookup key existing-query))
	 ;;gl.getQueryParameter(sphere.query, gl.QUERY_RESULT_AVAILABLE))
	 (when (gl.getQueryParameter existing-query gl.QUERY_RESULT_AVAILABLE)
		(if (gl.getQueryParameter existing-query gl.QUERY_RESULT)
			 (progn
				(hashmap-set visible-keys key 1)
				(set model:visible t))
			 (hashmap-set visible-keys key 0)
			
			 ))
	 (gl.beginQuery gl.ANY_SAMPLES_PASSED_CONSERVATIVE existing-query)
	 model:visible
 
  ))
(defmacro occlusion-query (key &rest body)
  `(let ((model:visible (occlusion-query::start ,key)))
	 ,@body
	 (gl.endQuery gl.ANY_SAMPLES_PASSED_CONSERVATIVE)
	 ))

(defmacro item(name &rest body)
  `(let ((item-name ,name))
	  (hashmap-set item-lookup item-name (mat4:clone model:transform))
	  ,@body))

(defun physics:bodies (list))

(defun physics:body (id)
  (push physics:bodies (list id model:transform))
  )


(defun fill-text (text)
  (let ((c-world (mat4:apply model:inverse-camera (mat4:apply model:transform (vec3:new 0 0 0))))
		  (console2 (mat4:apply model:projection c-world)))
	 (when (and
			  (<> 0 (vec3:z console2) 1.0001)
			  (<> -1 (vec3:x console2) 1.0)
			  (<> -1 (vec3:y console2) 1.0))
		
		(blit-ctx.fillText text (* 256 (+ 1 (vec3:x console2))) (* 256 (- 1 (vec3:y console2))))
		)))

(defmacro html-text (name)
  `(fill-text ,name))

(defvar item-bounds (make-hash-map))


(defun model:door-wall()
  ($ with-prefix model:)
  (offset -2 0 0.05
			 (scale 2 2.5 0.1
					  (upcube)))
  (offset 2 0 0.05
			 (scale 2 2.5 0.1
					  (upcube)))
  (offset 0 2.25 0.05
			 (scale 2 0.25 0.1
					  (upcube))))

(defun model:wall()
  ($ with-prefix model:)
  (offset 0 0 0.05
			 (scale 6 2.5 0.1
					  (upcube))))

(defun model:room ()
  ($ with-prefix model:)
  (wall)
  (rotate-y 0.01
			 (wall))
  (offset 0 0 4
			 (door-wall))
  (rgb 0.2 0.3 0.2
		 (offset 0 0 2
					(scale 4 0.1 4
							 (upcube)))))

(defvar offset-x 0.0)
(defvar offset-y 0.0)
(defvar rotation 0.0)
(defvar fps 15.0)
(defvar frame-id 0)
(defun animation-loop ()
  (incf frame-id)
  (set last-time time)
  (update-time)

  ;; handle events

  (key:clear-events)
  (when (key:down 'key:arrow-right)
	 (incf rotation -0.01)
	 )
  (when (key:down 'key:arrow-left)
	 (incf rotation 0.01)
	 )
  (let ((relative-move 0.0))
	 (when (key:down 'key:arrow-up)
	 
		(incf relative-move 0.1)
		)
	 (when (key:down 'key:arrow-down)
		(incf relative-move -0.1)
		)
	 (let ((id (mat4:identity)))
		(mat4:rotate-y id (* -1 2 math:pi rotation))
		(let ((v (vec3:mul-scalar (mat4:apply id (vec3:new 0 0 1)) relative-move)))
		  (incf offset-x (vec3:x v))
		  (incf offset-y (vec3:z v))
		  )))
  (set model:camera (mat4:identity))
  (mat4:translate model:camera (- offset-x) 2.5 (- offset-y))
  (mat4:rotate-y model:camera (* -2 math:pi rotation))
  (set model:inverse-camera (mat4:invert model:camera))
  
  ;(println offset-x offset-y)
  ($ let ((time2 (/ time 100))
			 (deltat (- time last-time))
			 (fps2 (/ 1.0 deltat))
			 ))
  (set fps (+ (* 0.9 fps) (* 0.1 fps2)))
  (set fps-display.innerHTML (concat ">> " (fps.toFixed 2) " " target-xn " " target-yn))

  ($ let ((pointer-pos (vec3:new 0 0 0))))
  (let ((v (list target-xn target-yn 1))
		  (v-rp (vec3:normalize (mat4:apply (mat4:invert model:projection) v)))
		  (v-rp-s (vec3:mul-scalar v-rp 5.0))
		  
		  (v2 (mat4:apply (mat4:* model:camera ) v-rp-s )))
	 ;(println 'processed:  v2)
	 (set pointer-pos v2))

  (set blit-ctx.font "12px Arial")
  (set blit-ctx.fillStyle "white")
  (blit-ctx.clearRect 0 0 blit-canvas.width blit-canvas.height)
	 
  (set physics:bodies (list))
  (model:start-gl-draw)
  (with-prefix model: 
	 (with-draw model:on-draw
		;($ rotate-y rotation)
		;($ offset offset-x 0 offset-y)
		(item "camera"
		(offset (vec3:x pointer-pos) (vec3:y pointer-pos) (vec3:z pointer-pos)
				  (item "cursor"
						  (html-text "cursor")
											

						  (rgb 1 0 1
								 (scale  0.2 0.2 0.2
											(cube)))))
		(rgb 1 0 1
			  (offset 0 2.5 (* 9 (math:sin (* 0.3 time)))
						 (scale 0.15 0.15 0.15
								  (rotate-y (* 0.4 time)
											 (cube)))))
		(rgb 0.3 0.3 0.6
			  
			  (offset 0 0 0
						 (scale 2 2 20
								  (physics:body 'physics:upcube)
								  (upcube))

						 

						 (offset 5 -2 -5
									(model:room))
						 (rgb 0.4 0.3 0.3
						 (offset 0 2 -2
									(scale 0.4 0.4 0.4
									(model:door-wall)))
						 (offset 0 2 -5
									(scale 0.4 0.4 0.4
									(model:door-wall)))
						 (offset 0 2 -8
									(scale 0.4 0.4 0.4
									(model:door-wall))))
						 (offset 3 0 3
						 (scale 0.2 1 0.2
						 (dotimes (i 20)
							(dotimes (j 20)
							  (model:rgb2 (case (% (+ i j) 3) (0 '(1 0 0)) (1 '(0 1 0))(2 '(0 0 1)))
											  (offset (* i 2) (* 0.3 (% (+ i j) 3)) (* j 2)
										 (scale 2 1 2
												  (tile))))))))

						 (offset 0 2 9.5
									(item "console"
											(occlusion-query "console"
																  (rgb 1 0 1
																		 (upcube))

											(when model:visible 
											  (html-text "console")))

											))
						 
						 (offset 0 10 9.5
									(item "console2" 
											(occlusion-query "console2"
																  (when model:visible
																	 (html-text "console2"))
											(rgb 1 0 1
												  (upcube)))))

						 )))))
	 (gl.bindVertexArray nil)
	 (set model::bound-va nil)
	 (println physics:bodies)
	 
	 
	 (requestAnimationFrame animation-loop))

(animation-loop)
