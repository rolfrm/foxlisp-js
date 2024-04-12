(defun polygon:new (vertices color)
    (let ((poly (list :vertices)))
        (set poly.triangleCount (/ (length vertices) 3))
        (if color
            (set poly.color (Float32Array.from color)) 
            (set poly.color (Float32Array.from vertices (lambda (x) 1.0)) )
        )

        (set poly.type 'polygon)
        (set poly.buffer nil)
        (set poly.colorBuffer nil)
        (set poly.vertices (float32-array-from vertices))

        poly)
)

(defun polygon:load (poly)
    (unless poly.buffer 
        (set poly.buffer (gl.createBuffer))
        (set poly.colorBuffer (gl.createBuffer))
        
        )
    
   (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
   (gl.bufferData gl.ARRAY_BUFFER poly.vertices gl.STATIC_DRAW)
   (gl.bindBuffer gl.ARRAY_BUFFER poly.colorBuffer)
   (gl.bufferData gl.ARRAY_BUFFER poly.color gl.STATIC_DRAW)
   ;(println 'loading: poly.vertices poly.color)
)

(defun polygon:delete (poly)   
    (when poly.buffer
        (gl.bindBuffer gl.ARRAY_BUFFER nil)
        (gl.deleteBuffer poly.buffer)
        (set poly.buffer nil))
)

(defun polygon:draw (poly)
    (unless poly.buffer 
        (polygon:load poly)
    )
    ;(println ">>> " poly.color poly.vertices)
    (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
    (gl.vertexAttribPointer 0 3 gl.FLOAT false 0 0)
    (gl.bindBuffer gl.ARRAY_BUFFER poly.colorBuffer)
    (gl.vertexAttribPointer 1 3 gl.FLOAT false 0 0)
    (gl.drawArrays gl.TRIANGLE_STRIP 0 poly.triangleCount)
    )

