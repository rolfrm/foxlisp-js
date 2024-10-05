(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")
(load "sdf.lisp")
(load "utils.lisp")
;; todo: Implement a camera transform
(defun get-time ()
  (%js "Date.now()"))

;; this part must be called to initialize gl
(model:initialize-gl "webgl-canvas")
(key:load-events model:webgl-canvas)

(defvar model:projection (mat4:perspective 1.6 1.0 0.1 1000.0))
(defvar model:projection (mat4:orthographic -30 30 -30 30 -100 100))

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
(defmacro item(name &rest body)
  `(let ((item-name ,name))
	  (hashmap-set item-lookup item-name (mat4:clone model:transform))
	  ,@body))

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

(defun green-box()
  ($ with-prefix model:)
  (rgb 1 0.5 0.7
		 (scale 1.2 1.2 1.2
				  (y-tile))))

(defun red-box()
  ($ with-prefix model:)
  (rgb 1 0.7 0.7
		 (scale 1.1 1.1 1.1
				  (y-tile))))
(defvar game-time 0.0)
(defun blades1 (obj)
  ($ let ((scaling (or (th obj 5) 1.0))))
  ($ with-prefix model:)
  ($ scale-uniform scaling)
  ($ rotate-z game-time)
  (rgb 1 1 1
		 (rotate-z (* 0.5 0.25) (y-tile))
		 (y-tile)))

(defvar game-objects (list (list 10 0 green-box -1 'player)
									(list 10 -5 red-box 0 'apple)
									(list 14 -15 red-box 1 'apple)
									(list 14 -15 red-box 2 'apple)
									(list 14 -15 red-box 3 'apple)
									(list 14 -15 red-box 4 'apple)
									(list 14 -15 red-box 5 'apple)
									(list 14 -15 red-box 6 'apple)
									
									(list 14 -15 red-box 7 'apple)
									(list 14 -15 red-box 8 'apple)
									(list 14 -15 red-box 9 'apple)
									(list 14 -15 red-box 10 'apple)
									(list 14 -15 red-box 11 'apple)
									(list 14 -15 red-box 12 'apple)
									
									(list 14 -15 red-box -1 'apple)
									(list 14 -25 red-box -1 'apple)
									(list 10 -18 red-box -1 'apple)
									(list 8 -3 red-box -1 'apple)
									(list 4 -4 red-box -1 'apple)
									
									(list -14 -15 red-box -1 'apple)
									(list 0 20 blades1 -1 'blade 2 'down-up 0.0)
									(list -10 20 blades1 -1 'blade 5)
									(list 10 20 blades1 -1 'blade 5)
									(list 20 20 blades1 -1 'blade 1 'down-up 0.0)
									))

(foreach x game-objects
			(set (th x 3) (or (th game-objects (th x 3)) nil)))

(defvar blood-objects (list))

(defun add-blood (x y)
  ($ let ((angle (math:random 0.0 (* 2 math:pi)))
			 (dx (math:sin angle))
			 (dy (math:cos angle))
			 (d (math:sqrt (+ (* dx dx) (* dy dy))))
			 ))
  (set dx (* 8.0 (/ dx d)))
  (set dy (* 3.0 (/ dy d)))
  (push blood-objects (list  (+ x (* dx 0.5)) (+ y (* dy 0.5)) dx dy (+ time 0.5))))
(defun load-blood(x y)
  (dotimes (i 10)
	 (add-blood x y)))
;(add-blood 0 0)


(defun animation-loop ()
  (incf frame-id)
  (set last-time time)
  (set game-time time)
  
  (update-time)
  ($ let ((delta-time (- time last-time))))
  ;; handle events

  (key:clear-events)
  ($ let ((y-move 0)
			 (x-move 0)
			 (speed 0.3)
			 ))
		  
  (when (key:down 'key:arrow-right)
	 (incf x-move 1))
  
  (when (key:down 'key:arrow-left)
	 (incf x-move -1))
  
  (when (key:down 'key:arrow-up)
	 (incf y-move 1))
  
  (when (key:down 'key:arrow-down)
	 (incf y-move -1))
  

  (let ((player-object (find game-objects (lambda (x) (th x 4)) 'player)))
	 ($ when player-object)
	 (let ((dmove (math:sqrt (+ (* x-move x-move) (* y-move y-move)))))
		(when (> dmove 0.01)
		  (incf (th player-object 0) (* speed (/ x-move dmove)))
		  (incf (th player-object 1) (* speed (/ y-move dmove))))))
  (foreach obj game-objects
			  (let ((type (th obj 4)))
				 (when (eq type 'apple)
					(let ((target (th obj 3)))
					  ($ when target)
					  (let ((tx (th target 0))
							  (ty (th target 1))
							  (ax (th obj 0))
							  (ay (th obj 1)))
						 (let ((dx (- tx ax))
								 (dy (- ty ay))
								 (d (math:sqrt (+ (* dx dx) (* dy dy))))
								 
								 )
							(set dx (/ dx d))
							(set dy (/ dy d))
							(when (< d 1.2)
							  (set dx 0))
							(when (< d 1.2)
							  (set dy 0))
							;(set dx (- dx (sign dx)))
							;(set dy (- dy (sign dy)))
							(set dx (clamp -1 dx 1))
							(set dy (clamp -1 dy 1))

							(set (th obj 0) (+ ax (* 0.3 dx)))
							(set (th obj 1) (+ ay (* 0.3 dy)))
							
							))))))

  ($ let ((connected-to-player
			  (lambda (x)
				 (when x
					(or (eq (th x 4) 'player) (connected-to-player (th x 3))))))))
  
  ;; objects repel eachother.
  ($ let ((blade-collisions (makehashmap))))
  ($ let ((connect-object (makehashmap))))
  (dotimes (i 10)
	 (foreach obj game-objects
				 ($ let ((x (th obj 0))
							(y (th obj 1))))
				 (foreach obj2 game-objects
							 ($ unless (eq (th obj2 4) 'blade))
							 ($ unless (eq obj2 obj))
							 ($ let ((x2 (th obj2 0))
										(y2 (th obj2 1))
										(size (or (th obj 5) 0.8))
										(dx (- x2 x))
										(dy (- y2 y))
										(d2 (+ (* dx dx) (* dy dy)))
										(d (if (< d2 10) (math:sqrt d2) d2))))
													
							 (when (and (< d size) (> d 0.001))
													 
							 (set dx (/ dx d))
							 (set dy (/ dy d))
							 (set (th obj2 0) (+ x2 (* 0.1 dx)))
							 (set (th obj2 1) (+ y2 (* 0.1 dy)))
							 (when (eq (th obj 4) 'blade)
								; blade collision
								(hashmap-set blade-collisions obj2 t)
								)
							 (when (and (eq (th obj2 4) 'apple)
											(not (connected-to-player obj2)))
							  
								(hashmap-set connect-object obj2 t)

								)
							 ))))
  (let ((keys (hashmap-keys connect-object))
		  (player-object (find game-objects (lambda (x) (th x 4)) 'player)))
	 (when (and player-object (> (length keys) 0))
		($ let ((connectome (makehashmap))
				  (last player-object)))
		(foreach x game-objects
					(when (th x 3)
					  (hashmap-set connectome (th x 3) x)))
		(loop (hashmap-get connectome last)
		 (set last (hashmap-get connectome last)))
		
		
		(foreach x keys
					(set (th x 3) last)
					(set last x)
			  
					)))
  
  (foreach x (hashmap-keys blade-collisions)
			  ($ let ((type (th x 4))))
			  (set (th x 3) nil)
			  (when (eq type 'player)
				 ($ let ((other (find game-objects (lambda (obj) (th obj 3)) x))))
				 (when other
					
					(set (th other 4) 'player)
					(set (th other 2) (th x 2))
					) 
			  ))
  
  (set game-objects (filter game-objects (lambda (x) (not (hashmap-get blade-collisions x)))))
  
  (foreach x (hashmap-keys blade-collisions)
			  (load-blood (th x 0) (th x 1)))

  (foreach b (filter game-objects (lambda (x) (th x 6)) 'down-up)
			  
			  (unless (th b 8)
				 (push b (th b 0))
				 (push b (th b 1)))
			  ($ let ((phase (th b 7))))
			  (set (th b 1) (+ (th b 9) (* 20.0 (- (math:sin phase) 1))))
			  (incf phase delta-time)
			  (set (th b 7) phase)
			  )
						  

  
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
		  (v-rp-s (vec3:mul-scalar v-rp 20.0))
		  
		  (v2 (mat4:apply (mat4:* model:camera ) v-rp-s )))
	 (set pointer-pos v2))

  (set blit-ctx.font "14px Arial")
  (set blit-ctx.fillStyle "white")
  (blit-ctx.clearRect 0 0 blit-canvas.width blit-canvas.height)
	 
  (model:start-gl-draw)
  (with-prefix model: 
	 (with-draw model:on-draw
		(item "camera"
				(offset (vec3:x pointer-pos) (vec3:y pointer-pos) (vec3:z pointer-pos)
						  (when nil
				  (item "cursor"
						  ;(html-text "cursor")
											

						  (rgb 1 0 1
								 (scale  0.5 0.5 0.5
											(cube))))))
				(when nil
				(offset-x 10
							 (scale 5 5 5
									  (blades1 time)))
				(offset-x -10
							 (scale 5 5 5
									  (blades1 time)))
				(scale 5 5 5
						 (blades1 time)))
				
		(foreach obj game-objects
					(offset (car obj) (cadr obj) 0
							  (funcall (th obj 2) obj)))
		(rgb 1 0 0
		(foreach blood blood-objects
					($ let ((x (th blood 0))
							  (y (th blood 1))
							  (dx (th blood 2))
							  (dy (th blood 3))
							  (spawn-time (th blood 4))))
					(set x (+ x (* (- time spawn-time) dx)))
					(set y (+ y (* (- time spawn-time) dy)))
					
					
					(offset x y 0
							  (y-tile))
					)
		(set blood-objects (filter blood-objects (lambda (x) (> (th x 4) time))))
		)
		


		)))
	 (gl.bindVertexArray nil)
	 (set model::bound-va nil)
	 
	 
	 (requestAnimationFrame animation-loop))

(animation-loop)
