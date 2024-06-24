
(defun vec3-interpolate (v a b)
  (vec3:add (vec3:mul-scalar a (- 1.0 v))
				(vec3:mul-scalar b v))

  )

(defvar wood-color-dark (vec3:new 0.3 0.3 0.5))
(defvar wood-color-light (vec3:new 0.8 0.6 0.4))

(defvar wood-foilage-dark (vec3:new 0.3 0.5 0.4))
(defvar wood-foilage-light (vec3:new 0.5 0.7 0.3))

(defvar ground-dark (vec3:new 0.3 0.4 0.35))
(defvar ground-light (vec3:new 0.45 0.6 0.3))

(defvar foilage-day '(0.5 1.0 0.5))

(defun tree (a height)
  (set a (or a 0.0))
  (set height (or height 4.0))
  ($ let ((treescale (* height 0.5))))

  (with-prefix model:
	 (offset 0.0 0 0.0
				(rgb2 (vec3-interpolate a wood-color-light wood-color-dark) 
					  ($ scale 1 height 1)
					  (upcube))
				(rgb2 (vec3-interpolate a wood-foilage-light wood-foilage-dark) 
						! offset 0 height 0
						! scale treescale treescale treescale
						(offset 0 -1 0
						(scale 1 1.9 1
								 (offset 0 1 0 
											(draw model::sphere12)))))
				)
	 ))

(defun flower (col)
  (with-prefix model:
	 (rgb2 col
			 (scale 0.2 0.2 0.2
					  (dotimes (i (math:random 1 4))
						 (offset (math:random -1.5 1.5) 0 (math:random -1.5 1.5)
								(tile)))
	 )
			 )))

(defun mushroom ()
  (with-prefix model:
	 (rgb 0.7 0.5 0.3
			(scale 0.4 0.2 0.4
					 (sphere12)
					 )
			(dotimes (i 4)
			  ! offset (math:random -0.3 0.3) 0.05 (math:random -0.3 0.3)
			(rgb 0.6 0.4 0.3
				  (offset 0 0.2 0
					  (scale 0.1 0.1 0.1
								(tile)
								))))
					(rgb 0.6 0.4 0.3
						  (offset 0 -0.2 0
					  (scale 0.2 0.4 0.2
								(sphere12)
								)))
  )))

(defun tree-modelling()
  ($ with-prefix model:)
  ($ offset 0 -2 0)
  (tree)
)

(defun high-bird (time)
  ($ with-prefix model:)
  ($ offset 0 3 0)
  ($ scale 0.5 0.5 0.5)
  ;; hat
  (rgb 0.5 0.3 0.2
  (scale 2.0 0.1 2.0
			(upcube))
  (scale 0.9 1.0 0.9
			(upcube)))
  ;; head
  (rgb 1 1 1
		 
		 ($ offset 0 -0.8 0)
		 ($ scale 1.0 1 0.8)
		 (sphere))
  ;; beak
  (rgb 1 1 0.4
		 ($ offset 0.8 -1 0)
		 ($ rotate 0.25 0 0 1)
		 ($ scale 0.5 0.9 0.5
			 (pyramid)))

  ;; eyes
  ($ let ((blink (* 10 (math:sin (* 2 time-component))))))
  (when (> (abs blink) 1)
	 (set blink 1))
  (rgb 0 0 0
		 ($ dotimes (i 2))
		 ($ offset 0 0 (- (* i 0.4) 0.2))
			
		 (offset 0.8 -0.3 0
					($ scale 0.2 (* blink 0.35) 0.2)
					($ bake)
					($ rotate 0.25 0 0 1)
					
					(model:tile-centered)))

  ;; coat
  (rgb 0.5 0.3 0.2
		 ;($ bake)
		 ($ offset 0 -1.6 0)
		 (scale 1.0 2 1.5
				  (downcube)
				  )
		 (rgb 0 0 0
				($ dotimes (i 2))
				($ offset 0.5 (+ -0.5 (* 0.8 (- i))) 0)
				($ scale 0.1 0.1 0.1)
				(cube))

		 )
  

  (dotimes (i 2)
	 ($ offset 0 0 (+ (* i 0.8) -0.4))
	 ($ rgb 1 1 0)
	 ($ offset 0 -3 0)
	 ($ rotate (* 0.1 (* (- i 0.5)) (math:sin (* 16 time))) 0 0 1)
	 ($ scale 0.2 2 0.2)
	 ( downcube))

  
  )


(defun high-bird-modelling ()
  ($ with-prefix model:)
  ($ offset 0 -3 0)
  ;($ scale 0.5 0.5 0.5)
  (high-bird 1.0)
  
  )
	 

(defun cultist ()
  ($ with-prefix model:)
  ($ rgb 1 1 1)
  ($ bake)
  ($ offset 0 1.4 0)
  ($ scale 1 1 1)
  ($ scale 0.8 0.8 0.8)
  ($ rgb 0.2 0.2 0.2)
  
  (offset 0 2 0
			 (scale 1.2 1.5 1.2
					  (pyramid)
					  ))
  ;; lower body 
  (offset 0 0 0
			 (downcube))
  
  (rgb 0 0 0
		 ($ dotimes (i 2))
		 ($ offset 0.5 1 (+ -0.2 (* 0.4 i)))
		 ($ scale 0.1 0.18 0.1)
		 (cube))
  
  (dotimes (i 2)
	 ($ offset 0 -1 (+ -0.25 (* 0.5 i)))
	 ($ scale 0.33 1 0.33)
	 (downcube))
  
  (rgb 0.7 0.6 0.6
		 ($ scale 1 2 1)
		 (upcube))
  )

(defun cultist-modelling ()
  ($ with-prefix model:)
  ($ offset 0 0 0)
  ;($ scale 0.5 0.5 0.5)
  (cultist 1.0)
  
  )
