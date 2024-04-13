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
		 ($ rotation 0.25 0 0 1)
		 ($ scale 0.5 0.9 0.5
			 (pyramid)))

  ;; eyes
  ($ let ((blink (* 10 (math:sin (* 2 time))))))
  (when (> (abs blink) 1)
	 (set blink 1))
  (rgb 0 0 0
		 ($ dotimes (i 2))
		 ($ offset 0 0 (- (* i 0.4) 0.2))
			
		 (offset 0.8 -0.3 0
					($ scale 0.2 (* blink 0.35) 0.2)
					($ bake)
					($ rotation 0.25 0 0 1)
					
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
	 ($ rotation (* 0.1 (* (- i 0.5)) (math:sin (* 16 time))) 0 0 1)
	 ($ scale 0.2 2 0.2)
	 ( downcube))

  
  )

(defun high-bird-modelling ()
  ($ with-prefix model:)
  ($ offset 0 1 0)
  ;($ scale 0.5 0.5 0.5)
  (high-bird)
  
  )
	 
