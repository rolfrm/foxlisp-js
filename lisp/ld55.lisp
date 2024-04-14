(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")
(load "ld55-models.lisp")
;; this part must be called to initialize gl
(defun get-element-by-id (item id)
    (item.getElementById id)
)

(defun get-context (item contextname)
    (item.getContext contextname)
)
(defvar webgl-canvas (get-element-by-id document "webgl-canvas"))

(defvar level-counter-obj (get-element-by-id document "level"))
(defvar distance-obj (get-element-by-id document "distance"))

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
       ;(println (keys:code-to-key evt.keyCode))
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

(defvar perspective (mat4:perspective 1.2 1.0 0.1 2000.0))
;(defvar perspective (mat4:orthographic -10 10 -10 10 -30 30))

(gl.enable gl.CULL_FACE)
(gl.cullFace gl.BACK)
(gl.enable gl.DEPTH_TEST)
(defvar poly-cache (makehashmap))
(defvar shader (shader:get-default))
(defun on-draw (model)
    
    (let ((cached (hashmap-get poly-cache model)))
        (unless cached
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
        (shader:set-color shader (vec3:x model:color) (vec3:y model:color) (vec3:z model:color) 1.0)
        (shader:set-model shader model:transform)
        (shader:set-model-view shader (mat4:multiply perspective model:transform))
        (polygon:draw cached)
        )  
	 )


(defvar bullets (list))
(defun shoot-bullet (loc dir)
  ;(println 'shoot-pew-pew loc dir)
  (push bullets (list loc dir 0))
  )

(defvar cultists (list

						)	

  )

(defvar nonrandom (list))
(dotimes (i 1000)
  (push nonrandom (math:random 0.0 1.0)))
(defun math:nonrandom(seed start stop)
  (let ((r (mod (abs (+ 1453241232 seed)) 1000)))
	 
	 (+ start (* (nth nonrandom r) (- stop start))))
	 )


(defun pentagram ()
  ($ with-prefix model:)
  ($ rgb 1 1 1)
  ($ bake)
  (progn
	 ($ dotimes (i 5))
	 ($ let ((r 5.0)
				(a (/ (+ i -0.25) -5.0))
				(a2 (* (/ i 5) math:2pi))
				))
	 ($ offset (* r (math:sin a2)) 0 (* r (math:cos a2)))
	 ($ rotate a 0 1 0)
	 ($ scale 0.1 0.1 (* r 2))
	 ($ offset 0 0 -0.5)
	 (upcube))
  (progn
	 ($ dotimes (i 10))
	 ($ let ((r 5.2)
				(a (/ (+ i 2.5) -10.0))
				(a2 (* (/ i 10) math:2pi))
				))
	 ($ offset (* r (math:sin a2)) 0 (* r (math:cos a2)))
	 ($ rotate a 0 1 0)
	 ($ scale 0.1 0.1 (* r 0.65))
	 ($ offset 0 0 -0.0)
	 (upcube))
  )


(defvar winangle (math:random 0.0 math:2pi))

(defvar win-dist 200)
(defvar winloc-x (* win-dist ! math:cos winangle))
(defvar winloc-y (* win-dist ! math:sin winangle))
(defvar map-seed (math:random 0 1000.0))
(defvar level-counter 1)
(defun reload-game ()
  (set level-counter-obj.innerHTML level-counter)
  ;(set win-dist (+ win-dist 100))
  (set distance-obj.innerHTML win-dist)
  (set map-seed (math:random 0 1000.0))

  (set winloc-x (* 400 ! math:cos winangle))
  (set winloc-y (* 400 ! math:sin winangle))
  (set poly-cache (makehashmap))
  (set model::baked-models (makehashmap))
  (set cultists (list))
  (set player-loc (vec3:new 0 0 0))
  (set player-charge 0.0)
  (dotimes (i 5)
	 (let ((r 5.5)
			 (a (* i (/ math:2pi 5.0))))
		
		(push cultists (list (vec3:new
									 
									 (+ winloc-x (* r (math:sin a))) 0 (+ winloc-y (* r (math:cos a)))) 0.0 a))
		
		))
  )

(defvar shapes (list))
(reload-game)

(defun hill(x y x0 y0 r h)
  (set x (- x x0))
  (set y (- y y0))
  (let ((dc (math:sqrt (+ (* x x) (* y y))))
		  (d (- r dc)))
	 (+ h (min 0 d))
  ))

(defvar audio-load-code "(p) => new Audio(p)")
(defvar audio-ctx-load-code "() => new (window.AudioContext || window.webkitAudioContext)();")
(defvar audio-context-new (js_eval audio-ctx-load-code))
(defvar audio-load-f (js_eval audio-load-code))
(defvar audioContext (audio-context-new))

(defvar walking-sound (audio-load-f "walking.mp3"))
;(defvar soundSrc (audioContext.createMediaElementSource walking-sound))
;(soundSrc.connect audioContext.destination)

(defvar background-sound (audio-load-f "forestambient.mp3"))
(defvar wowowow-sound (audio-load-f "wowowow.mp3"))
(defvar diiiu-sound (audio-load-f "diiiu.mp3"))


(defun set-loop(sound)
  (set sound.loop t))

(defun play-sound(sound)
  (sound.play)
  )

(defun stop-sound (sound)
  (set sound.currentTime 0.5)
  (sound.pause)
  )

(set-loop background-sound)
(set-loop walking-sound)
(defun heightmap (x y)
  (max
	(hill x y 0 0 10 2)
	;(hill x y 50 1 10 20)
	;(hill x y -20 -25 10 0)
	(hill x y winloc-x winloc-y 10 100)
  (+
	(* 2.0 (math:sin (+ map-seed (* x 0.3))) (math:cos ! + map-seed (* y 0.3)))
	(* 5.0 (math:sin (+ map-seed (* x 0.1))) (math:cos ! + map-seed  (* y 0.1)))
	(* 10.0 (math:sin (+ map-seed(* x 0.02))) (math:cos ! + map-seed  (* y 0.02)))
	(* 20.0 (math:sin (+ map-seed (* x 0.002))) (math:cos ! + map-seed (* y 0.002)))

	)))

;; rotate towards target. now and target are in turns.
(defun slow-turn (now target step)
  (let ((delta (- target now)))
    (if (> delta 0.5)
		  (- now (min step (- 1 delta)))
		  (if (< delta -0.5)
				(+ now (min step (+ 1 delta)))
		  		(if (< delta 0)
					 (- now (min step (- delta)))
					 (+ now (min step delta)))))))
					 
		
 

(defvar animate t)
(defvar time-component 15.0)
(defvar xrot 0.0)
(defvar yrot 0.0)
(defvar player-loc (vec3:new 0 0 0))
(defvar player-dir (vec3:new 1 0 0))
(defvar player-angle 0)
(defvar view-angle 0.0)
(defvar player-dist 0)
(defvar player-charge 0.0)
(defvar cam-loc (vec3:new 0 0 0))

(defun <> (min value max)
  (and (< min value) (> max value)))

(defun animation-loop ()
    (set time-component (+ time-component 0.01))
    (let ((shader (shader:get-default)))
        (shader:use shader)
		  )
	 ($ let ((move-vec (vec3:new 0 0 0)) (move-angle player-angle) (xrot 0.0)))
	 
    (when (key:down 'key:a)
		
      (set move-vec (vec3:new -1 0 0))
		(set move-angle 0.5)
		)
	 (when (or (key:down 'key:q) (key:down 'key:arrow-left))
		(incf view-angle 0.01))
	 (when (or (key:down 'key:e) (key:down 'key:arrow-right))
		(incf view-angle -0.01)
		)
    (when (key:down 'key:d)
		(set move-angle 0)
		(set move-vec (vec3:new 1 0 0)))
    (when (or (key:down 'key:arrow-up) (key:down 'key:w))
		(set move-angle -0.25)
      (set move-vec (vec3:new 0 0 -1)))
    (when (or (key:down 'key:s) (key:down 'key:arrow-down))
		(set move-angle 0.25)
      (set move-vec (vec3:new 0 0 1)))
	 (when (key:on-down 'key:space)
													 ;(play-sound walking-sound)
		
		(reload-game)
		)
	 (when (> player-charge 10.0)
		(play-sound diiiu-sound)
		(incf level-counter 1)
		(incf win-dist 100)
		(reload-game))
	 (if (> (vec3:length move-vec) 0.0)
		  (progn
			 (play-sound background-sound)
			 (play-sound walking-sound)
			 )
		  (stop-sound walking-sound))
	 (set move-vec (vec3:mul-scalar move-vec 0.3))
	 (when (> (vec3:length move-vec) 0)
		(set move-vec (mat4:apply (mat4:rotation (* (- move-angle view-angle) math:2pi) (vec3:new 0 1 0)) (vec3:new 0.3 0 0.0)))
		(set move-angle (- move-angle view-angle))
		)
    (set xrot move-angle)
	 (set player-angle move-angle)
	 (let ((next-loc (vec3:add player-loc move-vec))
			 (next-depth (heightmap (nth next-loc 0) (nth next-loc 2)))
			 )
		(when (> next-depth -8)
		  (set player-loc next-loc)))

	 
	 (set player-dist (+ player-dist (* 0.1 (vec3:length move-vec))))
    (setnth player-loc 1 (max -7 (heightmap (vec3:x player-loc) (vec3:z player-loc))))
	 ;(println 'player player-loc)
    (let ((r (mat4:rotation (* math:pi 2 move-angle) (vec3:new 0 1 0)))
          (ld (mat4:apply r (vec3:new 1 0 0)))
         )
		(when (> 2 (math:random 0 10))
		  
			 (shoot-bullet (vec3:add player-loc (vec3:new (math:random -30 30) 20 (math:random -30 30))) (mat4:apply r (vec3:new 0 -0.05 0)))
			 )
		)

	 (let ((xd (- winloc-x (nth player-loc 0)))
			 (yd (- winloc-y (nth player-loc 2)))
			 (d (math:sqrt (+ (* xd xd) (* yd yd)))))
		(when (< d 3.0)
		  (incf player-charge 0.02)
		  (play-sound wowowow-sound)
		  )
		)
	 
	 ;(println bullets)
	 (for-each bullet bullets
				  ($ let ((pos (car bullet))
							 (dir (cadr bullet))
							 (t (caddr bullet))))
				  (set pos (vec3:add pos dir))
				  (setnth pos 0 (+ (* 0.01 (math:sin (nth pos 1))) (nth pos 0))) 
				  (set t (+ t 1))
				  
				  (setnth bullet 0 pos)
				  (setnth bullet 2 t)
				  )
	 (set bullets (where bullets (lambda (x) (> (nth (nth x 0) 1) 4.0))))
	 (for-each cultist cultists
				  ($ let ((p (car cultist))
							 (d (vec3:sub player-loc p))
							 (dl (vec3:length d))
							 (dn (vec3:normalize d))
							 (a (/ (math:atan2 (vec3:z dn) (vec3:x dn)) math:2pi))
							 ))
				  
				  (setnth p 1 (heightmap (nth p 0) (nth p 2)))
				  (let ((target (if (< dl 15) a (getnth cultist 2))))
					 (when (> player-charge 0.0)
						(set target (/ player-charge 5.0)))
					 (setnth cultist 1 (slow-turn (getnth cultist 1) target 0.01))
				  
					 ))
	 (key:clear-events)
	 

    ;; lets make some funky clear-color based on time:
    (gl.clearColor 0.1 0.1 0.1 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (with-prefix model: 
    (with-draw on-draw    
        (rgb 1 0 1 
				 (offset 0.0 -3.0 -15.0
							
				 (rotation -0.10 1 0 0
            (rotation view-angle 0 1 0
            
							 (offset (- (vec3:x player-loc)) (- (vec3:y player-loc))  (- (vec3:z player-loc))

              (offset (vec3:x player-loc) (vec3:y player-loc) (vec3:z player-loc)
                ($ rotation xrot 0 1 0)
                (high-bird player-dist) 
					 )

				  (for-each cultist-npc cultists
							($ let ((pos (car cultist-npc))))
							($ offset (vec3:x pos) (vec3:y pos) (vec3:z pos))
							($ rotation (cadr cultist-npc) 0 1 0)
							(cultist))
				  (offset winloc-x (heightmap winloc-x winloc-y) winloc-y
							 (pentagram))
					
					;(println bullets)

					(for-each bullet bullets
								 ($ let ((pos (car bullet))))
								 ;(println pos)
								 ($ offset (vec3:x pos) (vec3:y pos) (vec3:z pos))
								 ($ rgb 0.3 0.8 0.3)
								 ($ scale 0.4 0.2 0.3)
								 (tile)
								 )
					

					(dotimes (offset -2 3)
					($ dotimes (offsety -5 2))
					(let (
          				(zone (+ offset (round (/ (nth player-loc 0) 40))))
							(zone2 (+ offsety (round (/ (nth player-loc 2) 40))))
							(zonei (+ 0.5 (* 0.5 (math:sin (/ zone 2.0))))))
							
					  
					  (rgb 1 1 1
							 (bake-keyed (+ (+ zone 1000) (* (+ zone2 1000) 10000))
											 (rgb2 (vec3-interpolate 0.0 ground-light ground-dark)
													 (draw
													  (list 'polygon :3d-triangle-strip
															  (gen-heightmap heightmap
																				  (+ (* zone 40))
																				  (+ (* zone2 40))
																				  (+ (* (+ zone 1) 40) -1)
																				  (+ (* (+ zone2 1) 40) 0)
																				  2)))
													 )
											 
					 (dotimes (i 10)
						($ let ((x (+ (math:random -20.0 20.0) (* zone 20 2)))
								  (y (+ (math:random -20.0 20.0) (* zone2 20 2)))
								  (s (math:random 1.0 1.3))))
						! when (> (heightmap x y) -4)
						(offset  x (heightmap x y) y
									(scale s s s
											 (tree zonei (math:random 4 12))
								  )

						))
					 
					 (dotimes (i 10)
						($ let ((x (+ (math:random -20.0 20.0) (* zone 20 2)))
								  (y (+ (math:random -20.0 20.0) (* zone2 20 2)))
								  (s1 (math:random 0.5 1.3))
								  (s2 (math:random 0.5 1.3))
								  (s3 (math:random 0.5 1.3))
								  (s4 (math:random 0.5 1.3))

								  ))
						(offset  x (heightmap x y) y
									(scale s1 s2 s3
											 (rgb 0.3 0.3 0.3
													(rotate s4 0 1 0
															  (sphere5))))
								  )

						)
					 (dotimes (i 20)
						! dotimes (j 3)
						
						($ let ((colors '((0.3 0.3 0.8) (0.8 0.8 0.3) (0.8 0.3 0.3)))
								  (x (+ (math:random -20.0 20.0) (* zone 20 2)))
								  (y (+ (math:random -20.0 20.0) (* zone2 20 2)))
								  (s1 (math:random 0.5 1.3))
								  (s2 (math:random 0.5 1.3))
								  (s3 (math:random 0.5 1.3))
								  (s4 (math:random 0.5 1.3))
								  (z (+ 0.02 (heightmap x y)))
								  ))
						($ when (> z -2.0))
						(offset  x z y
									(flower (nth colors j))

									))

					 (dotimes (i 10)
						
						($ let ((x (+ (math:random -20.0 20.0) (* zone 20 2)))
								  (y (+ (math:random -20.0 20.0) (* zone2 20 2)))
								  (z (+ 0.2 (heightmap x y)))
								  ))
						($ when (> z -2.0))
						(offset  x z y
									(mushroom)

									))
					 
					 
					 )
							 ;; draw arrow
					(dotimes (i 2)
					  ($ let ((x (+ (math:random -0.1 0.1)
										 (math:nonrandom (+ zone (* 13 zone2) (* i 19)) -20.0 20.0)
										 (* zone 20 2)))
								 (y (+ (math:random -0.1 0.1)
										 (math:nonrandom (+ zone (* 13 zone2) (* i 19)) -20.0 20.0)
										 (* zone2 20 2)))
								  (z (+ 0.2 (heightmap x y)))
								  ))
						($ when (> z -2.0))
						($ let ((dx (- winloc-x x))
								  (dy (- winloc-y y))
								  (d (math:sqrt (+ (* dx dx) (* dy dy))))
								  ))
						(set dx (/ dx d))
						(set dy (/ dy d))
						(rgb 0.9 0.9 1
							  (offset (+ x dx) (+ 1 z) (+ y dy)
										 (scale 0.2 0.2 0.2
												  (upcube))
										 )
							  (offset (+ x (* 0.66 dx)) (+ 1 z) (+ y (* 0.66 dy))
										 (scale 0.2 0.2 0.2
												  (upcube))
										 )
							  (offset x (+ 1 z) y
										 (scale 0.2 0.2 0.2
												  (upcube))
										 ))
						)
		 					 
							 
							 )
					  

					  ))
					(offset 0 -4 0
								($ rgb 0.3 0.3 0.7)
								($ scale 10000 10 10000)
								(downcube)
								)
					

					(offset 50 20 1
								($ rgb 0.2 0.2 0.2)
								($ scale 5 5 5)
													 ;(upcube)
								)
					 (offset -20 0 -25
								($ rgb 0.2 0.2 0.2)
								($ scale 10 10 10)
								;(upcube)
								)

                
                ))))))
    )
    (when animate 
        (requestAnimationFrame animation-loop)
    )
)




(defun modelling-loop ()
  (set time-component (+ time-component 0.01))
  (let ((shader (shader:get-default)))
    (shader:use shader)
    )

  (when (hashmap-get keydown 'key:a)
        (set xrot (- xrot 0.01)))

  (when (hashmap-get keydown 'key:d)
    (set xrot (+ xrot 0.01)))
  (when (hashmap-get keydown 'key:w)
        (set yrot (- yrot 0.01)))

  (when (hashmap-get keydown 'key:s)
    (set yrot (+ yrot 0.01)))



  (key:clear-events)
  
  (gl.clearColor 0.1 0.1 0.5 1.0)
  (gl.clear (+ gl.COLOR_BUFFER_BIT gl.DEPTH_BUFFER_BIT))
  (with-prefix model: 
    (with-draw on-draw
		(rgb 1 1 1
			  
			  (scale -500 -500 -500
						($ rgb 0.5 0.9 1.0)
						(sphere12))
		($ offset 0 0 -5)
		($ scale 1 1 1)
		($ rotation yrot 1 0 0)
		($ rotation xrot 0 1 0)
		;($ offset 0 -2 0)
		(progn ;bake
		  ($ scale 0.5 0.5 0.5)
		  (tree)
													 ;(cultist-modelling))

		)))
  
  (when animate
	 (requestAnimationFrame modelling-loop))
  
  ))

(animation-loop)
;(modelling-loop)
(defvar triangle (polygon:new
						 (list -1 -1 0
						 1 -1 0
						 -1 1 0
						 1 1 0)))
(defun sdf-loop  ()
  ! let ((shader ! shader:get-sdf))
  (shader:use shader)
  (gl.clearColor 0.1 0.1 0.5 1.0)
  (gl.clear ! + gl.COLOR_BUFFER_BIT gl.DEPTH_BUFFER_BIT)
  (polygon:draw triangle)

  ! requestAnimationFrame sdf-loop
  )
;(sdf-loop)
