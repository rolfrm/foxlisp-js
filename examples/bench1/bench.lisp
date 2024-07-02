(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")

(defun get-time ()
  (%js "Date.now()"))

;; this part must be called to initialize gl
(model:initialize-gl "webgl-canvas")
(defvar fps-display (document.getElementById "fps-display"))
(defvar time (/ (get-time) 1000.0))
(defvar last-time time)
(defun update-time()
  (set time (/ (get-time) 1000))
  )

(defvar fps 15.0)
(defun animation-loop ()
  (set last-time time)
  (update-time)
  
  ($ let ((time2 (/ time 100))
			 (deltat (- time last-time))
			 (fps2 (/ 1.0 deltat))
			 ))
  (set fps (+ (* 0.9 fps) (* 0.1 fps2)))
  (set fps-display.innerHTML (fps.toFixed 2))
	 (model:start-gl-draw)
	 (with-prefix model: 
    (with-draw model:on-draw    
      (rgb 1 0 1
			  ($ offset -0 -0 -25)
			  ($ rotate-y time2)
			  ($ rotate-x time2)
			  ($ rotate-z time2)
			  ($ rotate-x time2)
			  ($ rotate-y time2)
			  
			  (dotimes (i 50)
				 (dotimes (j 50)
					(dotimes (k2 50)
					($ offset (- i 25) (- j 25) (- k2 25))
					($ scale 0.25 0.8 0.25)
					($ rotate-y (* time 0.5 (/ j 15.0)))
					($ rgb (math:sin (* j 0.3))
						(math:cos (* k2 0.3))
						(+ 0.5 (* 0.5 (math:sin time2)))
						)
					(bake
					 ($ offset 0 -0.5 0)
					 (upcube)))))

			  
			  )))
	 (gl.bindVertexArray nil)
	 (set model::bound-va nil)
    (requestAnimationFrame animation-loop)
    
)

(animation-loop)
