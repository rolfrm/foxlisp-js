(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")
(load "ld55-models.lisp")
(load "sdf.lisp")
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

(defvar projection (mat4:perspective 1.2 1.0 0.01 2000.0))
;(defvar perspective (mat4:orthographic -10 10 -10 10 -30 30))

(gl.enable gl.CULL_FACE)
(gl.cullFace gl.BACK)
(gl.enable gl.DEPTH_TEST)
(defvar poly-cache (makehashmap))
(defvar shader (shader:get-default))
(shader:set-view shader projection)

(defvar bullets (list))
(defun shoot-bullet (loc dir)
  (push bullets (list loc dir 0)))

(defvar cultists (list))

(defvar nonrandom (list))
(dotimes (i 1000)
  (push nonrandom (math:random 0.0 1.0)))

(defun math:nonrandom(seed start stop)
  (let ((r (mod (abs (+ 1453241232 seed)) 1000)))
	 (+ start (* (nth nonrandom r) (- stop start)))))


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
	 (upcube)))


(defvar winangle (math:random 0.0 math:2pi))

(defvar win-dist 200)
(defvar winloc-x (* win-dist ! math:cos winangle))
(defvar winloc-y (* win-dist ! math:sin winangle))
(defvar map-seed (math:random -1000.0 1000.0))
(defvar level-counter 1)
(defun reload-game ()
  (model::resample-noise)
  (set zone-assets (makehashmap))
  (set level-counter-obj.innerHTML level-counter)
  ;(set win-dist (+ win-dist 100))
  (set distance-obj.innerHTML win-dist)
  (set map-seed (math:random -1000.0 1000.0))

  (set winloc-x (* win-dist ! math:cos winangle))
  (set winloc-y (* win-dist ! math:sin winangle))
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
  (println cultists))

(defvar shapes (list))
(reload-game)

(defun hill(x y x0 y0 r h)
  (set x (- x x0))
  (set y (- y y0))
  
  (let ((sqrd (+ (* x x) (* y y))))
	 (if (< sqrd (* (+ r h) (+ r h) 2))
		(let (
				(dc (math:sqrt sqrd))
				(d (- r dc)))
		  (+ h (min 0 d)))
		  -50)))
	

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
  (sound.play))

(defun stop-sound (sound)
  (set sound.currentTime 0.5)
  (sound.pause))

(set-loop background-sound)
(set-loop walking-sound)

(defun clamp(v1 v v2)
  (max v1 (min v v2)))

(defun heightmap-flatness (x y)
  (clamp 0.1 (+ 1 (* 2 (model:2dnoise (* 0.004 x) (* 0.004 y)))) 1.0))

(defun tree-density (x y)
  (* (heightmap-flatness x y) 10.0))

(defun is-flat(x y)
  (< (heightmap-flatness x y) 0.2))

(defun heightmap (x y)
  ($ let ((flatness (heightmap-flatness x y))))
  
  (max
	(hill x y 0 0 10 2)
	;(hill x y 50 1 10 20)
	;(hill x y -20 -25 10 0)
	(hill x y winloc-x winloc-y 10 100)
  (+	
	;(* 1.0 (model:noisef (+ map-seed (* x 0.5))) (model:noisef ! + map-seed (* y 0.5)))
	;(* 2.0 (+ (model:noisef (+ map-seed (* x 0.3))) (model:noisef ! + map-seed (* y 0.3))))
	;(* 5.0 (+ (model:noisef (+ map-seed (* x 0.1))) (model:noisef ! + map-seed  (* y 0.1))))
	;(* 10.0 (+ (model:noisef (+ map-seed (* x 0.05))) (model:noisef ! + map-seed  (* y 0.05))))
	;(* 20.0 (+ (model:noisef (+ map-seed (* x 0.025))) (model:noisef ! + map-seed (* y 0.025))))
	;(* 40.0 (+ (model:noisef (+ map-seed (* x 0.0125))) (model:noisef ! + map-seed (* y 0.0125))))
	;(* 2.0 ! model:2dnoise (* 0.3 x) (* 0.3 y))
	(* 1.0 ! model:2dnoise (* 0.2 x) (* 0.2 y))
	(* 2.0 ! model:2dnoise (* 0.07 x) (* 0.07 y))
	(* 2.0 ! model:2dnoise (* 0.035 x) (* 0.035 y))
	(* (* flatness 2.0) ! model:2dnoise (* 0.064 x) (* 0.064 y))
	(* (* flatness 4.0) ! model:2dnoise (* 0.032 x) (* 0.032 y))
	(* (* flatness 8.0) ! model:2dnoise (* 0.016 x) (* 0.016 y))
	(* (* flatness 12.0) ! model:2dnoise (* 0.008 x) (* 0.008 y))
	(* (* flatness 12.0) ! model:2dnoise (* 0.008 (+ 65512.31 x)) (* 0.008 (+ y 9535153.32)))
	(* (* flatness 12.0) ! model:2dnoise (* 0.008 (+ 312321.54 x)) (* 0.008 (+ y 355311.321)))
	;(* 32.0 ! model:2dnoise (* 0.004 x) (* 0.004 y))
	
  (* 0 (* 8.0 ! model:2dnoise (* 0.07 x) (* 0.07 y))
  (* 16.0 ! model:2dnoise (* 0.035 x) (* 0.035 y))	
  (* 16.0 ! model:2dnoise (* 0.016 x) (* 0.016 y))
	
	(* 32.0 ! model:2dnoise (* 0.008 x) (* 0.008 y))
	(* 32.0 ! model:2dnoise (* 0.004 x) (* 0.004 y))
	(* 32.0 ! model:2dnoise (* 0.002 x) (* 0.002 y))
	(* 32.0 ! model:2dnoise (* 0.001 x) (* 0.001 y))
	))))

(defun heightmap2 (x y)
  ($ let ((flatness (heightmap-flatness x y))))
  
  (max
	;(hill x y 0 0 10 100)
  (+	

	;(* 2.0 ! model:2dnoise (* 0.07 x) (* 0.07 y))
	;(* 2.0 ! model:2dnoise (* 0.035 x) (* 0.035 y))
	;(* (* flatness 2.0) ! model:2dnoise (* 0.064 x) (* 0.064 y))
	;(* (* flatness 4.0) ! model:2dnoise (* 0.032 x) (* 0.032 y))
	;(* (* flatness 8.0) ! model:2dnoise (* 0.016 x) (* 0.016 y))
	;(* (* flatness 10.0) ! model:2dnoise (* 0.032 x) (* 0.032 y))
	(* (* flatness 20.0) ! model:2dnoise (* 0.016 (+ 65512.31 x)) (* 0.016 (+ y 9535153.32)))
	(* (* flatness 50.0) ! model:2dnoise (* 0.008 (+ 312321.54 x)) (* 0.008 (+ y 355311.321))))))

(defun heightmap-colors(x y z)
  (if (< y -1)
		
		'(0.8 0.8 0.6)
		
		(if (> y 60)
			 (if (< (model:2dnoise (+ 10 x) (+ z 10)) 0.0)
				  '(0.7 0.7 0.7)
				  '(0.75 0.75 0.75))
			 (if (< (model:2dnoise x z) 0.0)
				  ground-dark
				  ground-light))))

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

(defvar getTime (js_eval "()=> Date.now()"))

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

(defvar last-time (getTime))

(defun draw-cultists()
  ($ with-prefix model:)
  (dotimes (i (length cultists))

	 ($ let ((cultist-npc (nth cultists i)) (pos (car cultist-npc))))
	 ($ offset (vec3:x pos) (vec3:y pos) (vec3:z pos))
	 ($ rotate (cadr cultist-npc) 0 1 0)
	 (cultist)))

(defvar zone-assets (makehashmap))

(defun zoneof(x y)
  ! let ((zone (floor (/ x 40)))
			(zone2 (floor (/ y 40)))
			(id (+ (+ 1000 zone) (* (+ zone2 1000) 10000))))
  id)

(defun get-zone-assets(zoneid)
  (let ((zonemap  (hashmap-get zone-assets zoneid)))
	 (unless zonemap
		(set zonemap (list))
		(set zonemap.trees (list))
		(set zonemap.rocks (list))
		(set zonemap.wisps (list (list 0 0) (list 0 0 1)))
		(hashmap-set zone-assets zoneid zonemap))
	 zonemap))

(defun animation-loop ()
  
    (let ((shader (shader:get-default)))
      (shader:use shader))
	 ($ let ((this-time (getTime)) (move-vec (vec3:new 0 0 0)) (move-angle player-angle) (xrot 0.0)
		 (delta 1.0)))
	 (set time-component  (+ time-component (* delta 0.01)))
    
	 (set last-time this-time)
    (when (key:down 'key:a)
      (set move-vec (vec3:new -1 0 0))
		(set move-angle 0.5))
	 
	 (when (or (key:down 'key:q) (key:down 'key:arrow-left))
		(incf view-angle (* 0.01 delta)))
	 
	 (when (or (key:down 'key:e) (key:down 'key:arrow-right))
		(incf view-angle (* delta -0.01)))
	 
    (when (key:down 'key:d)
		(set move-angle 0)
		(set move-vec (vec3:new 1 0 0)))

	 (when (or (key:down 'key:arrow-up) (key:down 'key:w))
		(set move-angle (* delta -0.25))
      (set move-vec (vec3:new 0 0 -1)))

	 (when (or (key:down 'key:s) (key:down 'key:arrow-down))
		(set move-angle (* delta 0.25))
      (set move-vec (vec3:new 0 0 1)))
	 (when (key:on-down 'key:r)
		(reload-game))
	 (when (> player-charge 8.0)
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
	 (set move-vec (vec3:mul-scalar move-vec (* delta 2)))
	 
	 (when (> (vec3:length move-vec) 0)
		(set move-vec (mat4:apply (mat4:rotation (* (- move-angle view-angle) math:2pi) (vec3:new 0 1 0)) (vec3:new 0.2 0 0.0)))
		(set move-angle (- move-angle view-angle))
		)
    (set xrot move-angle)
	 (set player-angle move-angle)
	 (setnth player-loc 1 (max -7 (+ 0 (heightmap (vec3:x player-loc) (vec3:z player-loc)))))
	 (set prev-loc player-loc)
	 (let ((next-loc (vec3:add player-loc move-vec))
			 
			 (next-depth (heightmap (nth next-loc 0) (nth next-loc 2))))
		(set player-loc next-loc)
		(when (< next-depth -8)
		  (set player-loc prev-loc)
		  )
		(let ((px (nth player-loc 0))
				(py (nth player-loc 2))
				(assets (get-zone-assets (zoneof px py))))
		  
		  
		  (for-each tree assets.trees
						(let ((x (nth tree 0)) (y (nth tree 1))
								(dx (- x px))
								(dy (- y py))
								(d (math:sqrt (+ (* dx dx) (* dy dy)))))
						  
						  (when (< d 1.0)
							 (println 'collision>> d x y player-loc)
						  (set player-loc prev-loc)
						  

						  )))

		  (for-each rock assets.rocks
						! let ((x (nth rock 0)) (y (nth rock 1))
								 (dx (- x px))
								 (dy (- y py))
								 (z (nth rock 2))
								 (d (- z (math:sqrt (+ (* dx dx) (* dy dy)))))
								 )
						! when (> d 0.0)
						(println 'rock! rock player-loc dx dy d)
						(setnth player-loc 1 (+ d (nth next-loc 1)))
						
						)
		  (dotimes (i (length assets.wisps))
			 ! let ((wisp (nth assets.wisps i))(x (nth wisp 0)) (y (nth wisp 1))
					  (dx (- x px))
					  (dy (- y py))
					  (d (math:sqrt (+ (* dx dx) (* dy dy))))
					  )
			 (when (< d 2.0)
				(setnth wisp 2 0)
				)

			 )

		  ))
	 
	 	 
	 (set player-dist (+ player-dist (* 0.1 (vec3:length move-vec))))
    
;	 (setnth player-loc 1 50)
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
		  (incf player-charge (* delta 0.02))
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
	 ;(println cultists)
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
    (gl.clearColor 1.0 1.0 1.0 1.0)
    ;(gl.clearColor 0.0 0.0 0.0 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (with-prefix model: 
		(with-draw model:on-draw
		  
        (rgb 1 0 1 
				 (offset 0.0 -5.0 -10.0
							
				 (rotate-x -0.02
            (rotate-y view-angle
            
							 (offset (- (vec3:x player-loc)) (- (vec3:y player-loc))  (- (vec3:z player-loc))

              (offset (vec3:x player-loc) (vec3:y player-loc) (vec3:z player-loc)
                ($ rotate-y xrot)
                (high-bird player-dist) 
					 )
				  
				  
				  (draw-cultists)
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
					

					(dotimes (offset -10 11)
					  ($ dotimes (offsety -10 11))
					  
					  (let (
							  (chunk-size 40)
          				(zone (+ offset (floor (/ (nth player-loc 0) chunk-size))))
							(zone2 (+ offsety (floor (/ (nth player-loc 2) chunk-size))))
							(zonei (+ 0.5 (* 0.5 (math:sin (/ zone 2.0)))))
							  (zoneid (+ (+ zone 1000) (* (+ zone2 1000) 10000)))
							  (chunk-x (* zone chunk-size))
							  (chunk-y (* zone2 chunk-size))
							(assets (get-zone-assets zoneid))
							)
					  
							
					  
					  (rgb 1 1 1
							 (bake-keyed zoneid
											 (rgb2 (vec3-interpolate 0.0 ground-light ground-dark)
													 (draw
													  (gen-heightmap (lambda (x y)
																			 (let ((z (heightmap x y)))
																				(if (< z -0.3)
																					 -100
																					 (if (> (model:2dnoise (* 0.01 x) (* 0.01 y)) -0.6) z -100)
																					 
																					 )))
																				  (+ (* zone chunk-size))
																				  (+ (* zone2 chunk-size))
																				  (+ (* (+ zone 1) chunk-size) 0)
																				  (+ (* (+ zone2 1) chunk-size) 0)
																				  1
																				  )
													  )

													 )
											 (rgb2 (vec3:new 0.9 0.9 1)
													 (draw
													  (gen-heightmap (lambda (x y)
																			 (let ((z (+ 0.3 (heightmap x y))))
																				(if (< z -0.3)
																					 -100
																					 (if (< (model:2dnoise (* 0.01 x) (* 0.01 y)) -0.5) z -100)
																					 )))
																				  (+ (* zone chunk-size))
																				  (+ (* zone2 chunk-size))
																				  (+ (* (+ zone 1) chunk-size) 0)
																				  (+ (* (+ zone2 1) chunk-size) 0)
																				  1
																				  ;heightmap-colors
																				  )
													  )

													 )
											 (rgb2 (vec3:new 0.8 0.8 0.5)
													 (draw
													  (gen-heightmap (lambda (x y)
																			 (let ((z (heightmap x y)))
																				(if (> z 0)
																					 0
																					 (+ z 0.2))
																				))
																				  (+ (* zone chunk-size))
																				  (+ (* zone2 chunk-size))
																				  (+ (* (+ zone 1) chunk-size) 0)
																				  (+ (* (+ zone2 1) chunk-size) 0)
																				  1
																				  ;heightmap-colors
																				  )
													  )

													 )

											 
					 (dotimes (i (floor (tree-density (* zone chunk-size) (* zone2 chunk-size))))
						($ let ((x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
								  (s (math:random 1.0 1.3))))
						(when (and (> (heightmap x y) -4) (< (heightmap x y) 63))
						  (offset  x (heightmap x y) y
									  (scale s s s
												(tree (+ (math:random -0.3 0.3) zonei) (math:random 4 12))
												)
									  
									  )
						  (push assets.trees (list x y))
						))
					 
					 (dotimes (i 10)
						($ let ((x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
								  (s1 (math:random 0.5 3.3))
								  (s2 (math:random 0.5 3.3))
								  (s3 (math:random 0.5 3.3))
								  (s4 (math:random 0.0 1.0))

								  ))
						(push assets.rocks (list x y s2))
						(offset  x (- (heightmap x y) 0.2) y
									(scale s1 s2 s3
											 (rgb 0.3 0.3 0.3
													(rotate-y s4 (sphere5)
															  )))
								  )
						)
					 
					 (when (and (< (math:random 0 40) 1.0) (is-flat chunk-x chunk-y))
						
						($ let ((x (+ chunk-x (math:random 0 chunk-size)))
								  (y (+ chunk-y (math:random 0 chunk-size)))))
						(println 'house x y)
						(rgb 0 0 0
							  ($ offset x (heightmap x y) y)
							  ($ scale 4 100 4)
							  (upcube)

							  ))

					 (dotimes (i 3)
						($ let ((x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
								  (s1 (math:random 4.5 8.3))
								  (s2 (math:random 4.5 8.3))
								  (s3 (math:random 4.5 8.3))
								  (s4 (math:random 4.5 8.3))
								  (smax (max s1 s2 s3))
								  (z (- (heightmap x y) (* smax 0.2)))
								  ))
						! unless (< (heightmap x y) -1)
						(offset  x z y
									(scale s1 s2 s3
											 (rgb 0.3 0.5 0.25
													(rotate-y s4 1
															  (sphere5)
															  )))
								  )

						)
					 (dotimes (i 3)
						($ let ((x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
								  (s1 (math:random 4.5 8.3))
								  (s2 (math:random 4.5 8.3))
								  (s3 (math:random 4.5 8.3))
								  (s4 (math:random 4.5 8.3))

								  ))
						(push assets.rocks (list x y (* 0.3 s2)))
						(offset  x (- (heightmap x y) (* s2 0.7)) y
									(scale s1 s2 s3
											 (rgb 0.5 0.5 0.5
													(rotate-y s4
															  (sphere5)
															  )))
								  )

						)
					 
					 (dotimes (i 20)
						! dotimes (j 3)
						
						($ let ((colors '((0.3 0.3 0.8) (0.8 0.8 0.3) (0.8 0.3 0.3)))
								  (x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
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
						
						($ let ((x (+ (math:random 0.0 chunk-size) (* zone chunk-size)))
								  (y (+ (math:random 0.0 chunk-size) (* zone2 chunk-size)))
								  (z (+ 0.2 (heightmap x y)))
								  ))
						($ when (> z -2.0))
						(offset  x z y
									(mushroom)

									))
					 
					 
					 )

							 (offset (* zone 20 2) -4 (* zone2 20 2)
								($ rgb 0.3 0.3 0.7)
								($ scale chunk-size 10 chunk-size)
								(tile)
								)
					
							 ;; draw arrow
					(dotimes (i (length assets.wisps))
					  ($ let ((wisp (nth assets.wisps i))
								 (x (+ (math:random -0.1 0.1)
										 (math:nonrandom (+ zone (* 13 zone2) (* i 19)) 0.0 chunk-size)
										 (* zone chunk-size)))
								 (y (+ (math:random -0.1 0.1)
										 (math:nonrandom (+ zone (* 13 zone2) (* i 19)) 0.0 chunk-size)
										 (* zone2 chunk-size)))
								  (z (+ 0.2 (heightmap x y)))
								 ))
					  
					  ($ when (and (nth wisp 2) (> z -2.0)))
					  (setnth wisp 0 x)
					  (setnth wisp 1 y)
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
										 ))))))

                
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
  
  (gl.clearColor 0.0 0.0 0.0 1.0)
  (gl.clear (+ gl.COLOR_BUFFER_BIT gl.DEPTH_BUFFER_BIT))
  (with-prefix model: 
    (with-draw on-draw
		(rgb 1 1 1
			  
			  (scale -500 -500 -500
						($ rgb 0.5 0.9 1.0)
						(sphere12))
		($ offset 0 0 -80)
		($ scale 1 1 1)
		($ rotate (+ -0.2 yrot) 1 0 0)
		($ rotate xrot 0 1 0)
		;($ offset 0 -2 0)
		(progn ;bake
		  ($ scale 0.4 0.4 0.4)
		  (rgb 1 1 1
				 
				 )
		  (bake
			(rgb 1 0 1
				  
				  
			(when t (draw
			 
			 (gen-heightmap heightmap
										 (+ (* -20 10))
										 (+ (* -20 10))
										 (+ (* (+ 20 1) 10) -1)
										 (+ (* (+ 20 1) 10) 0)
										 0.5
										 (lambda (x y z)
											
											(vec3:mul-scalar
											 (vec3:new 0.2 0.0 (* 6.0 (+ 1.0 (math:cos (* 2.0 y)))))
											 (* (+ y 50) 0.01)

											 ))

										 ))))
			
													 ;(tree)
													 ;(cultist-modelling))

		))))
  
  (when animate
	 (requestAnimationFrame modelling-loop))
  
  ))

(animation-loop)
;(modelling-loop)

(defvar model1 0)
(set xrot 1.0)
(defvar zoomt -10)
(defvar rotm (mat4:identity))
(defun sdf-loop  ()
  (unless model1
	 (let ((pts (list)) (sizes (list)) (color (list))
			 (sdf (lambda (p) (min
									 (sphere p (vec3:new 0 0 -1.2) 0.15)
									 (sphere p (vec3:new 0 -10 0.0) 10)
										  (sphere p (vec3:new 0 0 0) 0.5)
										  (sphere p (vec3:new 0 0 1) 0.7)
										  (sphere p (vec3:new 0 0 2) 1.2)
										  ))))
		(sdf-points pts sizes sdf (vec3:new 0 0 0) 10.0 0.01)
		(dotimes (i (length pts))
		  (let ((p (nth pts i))
				  (c (vec3:new 1 1 0))
				  )
			 (when (< (sphere p (vec3:new 0 0 -1.2) 0.15) 0.0)
				  (set c (vec3:new 0 0 1))
				  )
			 (when (< (sphere p (vec3:new 0 -10 0.0) 10) 0.0)
				  (set c (vec3:new 0 1 0))
				  )
			 (when (< (sphere p (vec3:new 0 0 2.0) 1.2) 0.0)
				  (set c (vec3:new 1 0 0))
				  )
			 (when (< (sphere p (vec3:new 0 0 1.0) 0.7) 0.0)
				(set c (vec3:new 1 0 1))
				)
			 (push color (nth c 0))
			 (push color (nth c 1))
			 (push color (nth c 2))

		  ))
		(set model1 (polygon:new-points (float32-array-flatten pts)
												  (Float32Array.from sizes)
												  (Float32Array.from color)))
		(println 'sizes: model1.sizes.length)
		(println 'model1 model1)
		))

  (set xrot 0.0)
  (set yrot 0.0)
  (when (hashmap-get keydown 'key:a)
        (set xrot (- xrot 0.04)))

  (when (hashmap-get keydown 'key:d)
    (set xrot (+ xrot 0.04)))
  (when (hashmap-get keydown 'key:w)
        (set yrot (- yrot 0.04)))

  (when (hashmap-get keydown 'key:s)
    (set yrot (+ yrot 0.04)))

  (when (hashmap-get keydown 'key:arrow-up)
    (set zoomt (- zoomt 0.04)))
  (when (hashmap-get keydown 'key:arrow-down)
    (set zoomt (+ zoomt 0.04)))

  
  
  (when xrot
	 (let ((rot (mat4:rotation xrot (vec3:new 0 1 0))))
		(set rotm (mat4:multiply rot rotm ))
	 ))
  (when yrot
	 (let ((rot (mat4:rotation yrot (vec3:new 1 0 0))))
		(set rotm (mat4:multiply rot rotm ))
	 ))

  
  
  ! let ((shader ! shader:get-sdf)
			(perspective (mat4:perspective 1.2 1.0 0.01 2000.0))
			(tr (mat4:translate 0 0 zoomt))
			(m (mat4:multiply tr (mat4:multiply rotm (mat4:scaling 0.4 0.4 0.4))))
			)

  
  (shader:use shader)
  (shader:set-model shader m)
  (shader:set-model-view shader (mat4:multiply perspective m))
  (gl.clearColor 0.1 0.1 0.5 1.0)
  (gl.clear ! + gl.COLOR_BUFFER_BIT gl.DEPTH_BUFFER_BIT)
  
  (polygon:draw model1)

  ! requestAnimationFrame sdf-loop
  )


;(sdf-loop)
