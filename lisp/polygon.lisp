(defun polygon:new (vertices)
    (let ((poly (list :vertices)))
        (println 'new-polygon vertices)
        (set poly.type 'polygon)
        (set poly.buffer nil)
        (set poly.vertices (apply float32-array vertices))
        (set poly.triangleCount (/ (length vertices) 3))
        poly)
)

(defun polygon:load (poly)
    (unless poly.buffer 
        (set poly.buffer (gl.createBuffer)))
    
   (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
   (gl.bufferData gl.ARRAY_BUFFER poly.vertices gl.STATIC_DRAW)
   (println 'loading: poly.vertices)
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
    (gl.bindBuffer gl.ARRAY_BUFFER poly.buffer)
    (gl.vertexAttribPointer 0 3 gl.FLOAT false 0 0)
    (gl.drawArrays gl.TRIANGLE_STRIP 0 poly.triangleCount)
    )

