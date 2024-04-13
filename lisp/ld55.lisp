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

(defvar perspective (mat4:perspective 1.5 1.0 0.1 1000.0))

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
  (println 'shoot-pew-pew loc dir)
  (push bullets (list loc dir 0))
  )

(defvar cultists (list

						)	

  )

(defun pentagram ()
  ($ with-prefix model:)
  ;($ bake)
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

  
  

(defvar shapes (list))

(dotimes (i 5)
  (let ((r 5.5)
		  (a (* i (/ math:2pi 5.0))))
	 (push cultists (list (vec3:new (* r (math:sin a)) 0 (* r (math:cos a))) 0.0))

  ))



(defvar animate t)
(defvar time-component 15.0)
(defvar xrot 0.0)
(defvar yrot 0.0)
(defvar player-loc (vec3:new 0 0 0))
(defvar player-dir (vec3:new 1 0 0))
(defvar player-angle 0)
(defvar player-dist 0)
(defvar cam-loc (vec3:new 0 0 0))
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
    (when (key:down 'key:d)
		(set move-angle 0)
		(set move-vec (vec3:new 1 0 0)))
    (when (key:down 'key:w)
		(set move-angle -0.25)
      (set move-vec (vec3:new 0 0 -1)))
    (when (key:down 'key:s)
		(set move-angle 0.25)
      (set move-vec (vec3:new 0 0 1)))
	 (set move-vec (vec3:mul-scalar move-vec 0.3))
    (set xrot move-angle)
	 (set player-angle move-angle)
	 (set player-loc (vec3:add player-loc move-vec))
	 (set player-dist (+ player-dist (* 0.1 (vec3:length move-vec))))
    
    (let ((r (mat4:rotation (* math:pi 2 move-angle) (vec3:new 0 1 0)))
          (ld (mat4:apply r (vec3:new 1 0 0)))
         )
		  (when (key:on-down 'key:space)
			 (shoot-bullet (vec3:add player-loc (vec3:new 0 2 0)) (mat4:apply r (vec3:new 2 0 0)))
			 )
		  )
	 (for-each bullet bullets
				  ($ let ((pos (car bullet))
							 (dir (cadr bullet))
							 (t (caddr bullet))))
				  (set pos (vec3:add pos dir))
				  (set t (+ t 1))
				  (setnth bullet 0 pos)
				  (setnth bullet 2 t)
				  )
	 (for-each cultist cultists
				  ($ let ((p (car cultist))
							 (d (vec3:sub player-loc p))
							 (dn (vec3:normalize d))
							 (a (math:atan2 (vec3:z dn) (vec3:x dn)))
							 ))
					  (setnth cultist 1 (/ a (* 2 math:pi)))
				  
					  )
	 (key:clear-events)
	 

    ;; lets make some funky clear-color based on time:
    (gl.clearColor 0.1 0.1 0.1 1.0)
    (gl.clear gl.COLOR_BUFFER_BIT)
    (with-prefix model: 
    (with-draw on-draw    
        (rgb 1 0 1 
				 (offset 0.0 -2.0 -10.0
							
				 (rotation -0.1 1 0 0
            (rotation 0 0 1 0
            
              (offset (- (vec3:x player-loc)) (- (vec3:y player-loc))  (- (vec3:z player-loc))
							 (scale -500 -500 -500
									  ($ rgb 0.5 0.9 1.0)
									  (sphere12))

              (offset (vec3:x player-loc) (vec3:y player-loc) (vec3:z player-loc)
                ($ rotation xrot 0 1 0)
                (high-bird player-dist) 
					 )
				  (for-each cultist-npc cultists
							($ let ((pos (car cultist-npc))))
							($ offset (vec3:x pos) (vec3:y pos) (vec3:z pos))
							($ rotation (cadr cultist-npc) 0 1 0)
							(cultist))
				  (pentagram)
					
					(println bullets)

					(for-each bullet bullets
								 ($ let ((pos (car bullet))))
								 ;(println pos)
								 ($ offset (vec3:x pos) (vec3:y pos) (vec3:z pos))
								 ($ rgb 1 1 1)
								 ($ scale 0.2 0.2 0.2)
								 (sphere12)
								 )
					

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
						($ scale (math:random 5 20) (math:random 5 10) (math:random 5 10))
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
		 (cultist-modelling))

		)))
  
  (when animate
	 (requestAnimationFrame modelling-loop))
  
  )

(animation-loop)
;(modelling-loop)
