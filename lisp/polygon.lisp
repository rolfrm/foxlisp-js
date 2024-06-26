(defun polygon:new (vertices color)
    (let ((poly (list :vertices)))
        (set poly.triangleCount (/ (length vertices) 3))
        (if color
            (set poly.color (Float32Array.from color)) 
            (set poly.color (Float32Array.from vertices (lambda (x) 1.0))))

        (set poly.type 'polygon)
        (set poly.buffer nil)
        (set poly.colorBuffer nil)
        (set poly.vertices (float32-array-from vertices))

        poly))

(defun polygon:new-points (vertices sizes color)
  (let ((poly (list :points)))
	 (set poly.count (/ (length vertices) 3))
	 (set poly.color color)
	 (set poly.vertices vertices)
	 (set poly.sizes sizes)
	 (set poly.colorBuffer nil)
	 (set poly.buffer nil)
	 (set poly.type 'points)
	 (set poly.sizeBuffer nil)
	 poly))

(defun polygon:load (poly)
  (unless poly.buffer
	 (set poly.buffer (gl.createBuffer))
    (set poly.colorBuffer (gl.createBuffer))
	 (when (eq poly.type 'points)
		(set poly.sizeBuffer (gl.createBuffer))
		)
    )
  (set polygon::bound-buffer poly.buffer)
		
   (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
   (gl.bufferData gl.ARRAY_BUFFER poly.vertices gl.STATIC_DRAW)
   (gl.bindBuffer gl.ARRAY_BUFFER poly.colorBuffer)
   (gl.bufferData gl.ARRAY_BUFFER poly.color gl.STATIC_DRAW)
	(when poly.sizeBuffer
	  (gl.bindBuffer gl.ARRAY_BUFFER poly.sizeBuffer)
     (gl.bufferData gl.ARRAY_BUFFER poly.sizes gl.STATIC_DRAW)))

(defun polygon:delete (poly)   
    (when poly.buffer
        (gl.bindBuffer gl.ARRAY_BUFFER nil)
        (gl.deleteBuffer poly.buffer)
        (set poly.buffer nil)))

(defvar polygon::bound -1)
(defun polygon:draw (poly)
  (unless poly.buffer
    (polygon:load poly))
  (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
  (gl.vertexAttribPointer 0 3 gl.FLOAT false 0 0)
  (gl.bindBuffer gl.ARRAY_BUFFER poly.colorBuffer)
  (gl.vertexAttribPointer 1 3 gl.FLOAT false 0 0)
    
	 (if poly.sizeBuffer
		  (progn
			 (gl.bindBuffer gl.ARRAY_BUFFER poly.sizeBuffer)
			 (gl.vertexAttribPointer 2 1 gl.FLOAT false 0 0)
			 (gl.drawArrays gl.POINTS 0 poly.count)
			 )
		  (gl.drawArrays gl.TRIANGLE_STRIP 0 poly.triangleCount)
		  ))

