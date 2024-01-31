(load "lisp.lisp")


(defun new-f32-array(n)
  (%js "new Float32Array(" n ")")
  )

(defun f32-array->u8-array(array)
  
  (%js "new Uint8Array(" array.buffer ")")
  )

(defun f32-array->u32-array(array)
  
  (%js "new Uint32Array(" array.buffer ")")
  )

(defun int->float (x)
  (%js "new Number(" x ")")
  )

(defun f32-array (&rest values)
  (let ((len (length values)))
	 (let ((array (new-f32-array len) )) 
	   (for i 0 (< i len) (incf i)
		     (setnth array i (getnth values i))
			  )
		array
		)  
	 )
  )

(defun vec3 (x y z)
  (let ((array (%js "new Float32Array(3)")))
	 (setnth array 0 x)
	 (setnth array 1 y)
	 (setnth array 2 z)
	 array
	 )
  )
(defun vec3-x (v) (getnth v 0))
(defun vec3-y (v) (getnth v 1))
(defun vec3-z (v) (getnth v 2))

(defun vec3-combine (a b f)
  (vec3 
	(f (vec3-x a) (vec3-x b))
	(f (vec3-y a) (vec3-y b))
	(f (vec3-z a) (vec3-z b))
	)
  )

(defun vec3-apply (a f)
  (vec3 
	(f (vec3-x a))
	(f (vec3-y a))
	(f (vec3-z a))
	)
  )

(defvar sqrt Math.sqrt)
(defun vec3-length (v) 
  (let ((x (vec3-x v)) (y (vec3-y v)) (z (vec3-z v)))
	 (sqrt (+ (* x x) (* y y) (* z z)))
	 )
  )

(defun vec3-normalize (v) (vec3-scale v (vec3-length v)))


(defun vec3-sub(a b)
  (vec3-combine a b (lambda (x y) (- x y)))
  )
(defun vec3-add(a b)
  (vec3-combine a b (lambda (x y) (+ x y)))
  )

(defun vec3-scale(a s)
  (vec3-apply a (lambda (x) (* x s)))
  )

(defun vec3-dot(a b)
   (+ (* (vec3-x a) (vec3-x b))
      (* (vec3-y a) (vec3-y b))
	  (* (vec3-z a) (vec3-z b)))

)

(defun blit-pixels (img f)
  (let ((w img.width) (h img.height) (data img.data)
		  (color (list 0 0 0 255)) )
	 (for i 0 (< i h) (incr i)
			(for j 0 (< j w) (incr j)
				  
				  (let ((color2 (f color j i))
						  (index (* 4 (+ j (* i w) ))))
					 (setnth data index (getnth color 0))
					 (setnth data (+ 1 index) (getnth color 1))
					 (setnth data (+ 2 index) (getnth color 2))
					 (setnth data (+ 3 index) (getnth color 3))
					 )
				  )
			)
	 
	 )
  )


(defun sphere(p r c )
  (- (vec3-length (vec3-sub p c)) r))

(defun get-color (rgb x y)
													 ;(println x y)
  (let ((d (vec3 0.0 -1.0 0.0))
	     (p (vec3 (- x 256.0) 256.0 (- y 256.0)))
		  (done 0) )
	 
    (loop (not done)
	  (let ((c (sphere p 15.0 (vec3 0.0 0.0 0.0))))
													 ;(println c)

	    (if (< c 0.01)
		     (progn 
		       (setnth rgb 1 255)
				 (set done t)
				 )
			  (progn 
				 (setnth rgb 1 0)
				 (let ((new-p (vec3-add (vec3-scale d c) p)))
					(set p new-p))
				 (when (> c 3000.0)
				   (set done t)
					)
				 )
			  ))
     ))
  )

(let ((test (list 0 0 0 0)))

  (get-color test 300 256)
  )



(when 0
  (let ((canvas (document.getElementById "thecanvas")))
	 (unless canvas
		(println "creating canvas")
		(set canvas (document.createElement "canvas"))
		(set canvas.height 400)
		(set canvas.width 400)
		(set canvas.id "thecanvas")
		(document.body.appendChild canvas)
		)
	 (when 0
		(let ((ctx (canvas.getContext "2d")))
		  (let ((dst (ctx.createImageData 512 512)))
			 (blit-pixels dst get-color)
			 (ctx.putImageData dst 0 0)
			 
			 )	
		  
		  ))
	 (let ((gl (canvas.getContext "webgl2")))
		(let ((vertex-source "#version 300 es
	  	in vec4 vert;
		out float v;
		out float v2;
                void main(void) {
				     v = vert.z;
					 v2 = float(gl_VertexID/2) ;
                    gl_Position = vec4(vert.x * 0.2 - 1.0, vert.y * 0.2, 0.0, 1.0);
                }
	")
				(fragment-source "#version 300 es
	   precision mediump float;
vec3 getColor(int index) {
   index = int(mod(float(index), 6.0));
   if(index == 0) return vec3(1, 0, 0);
   if(index == 1) return vec3(0, 1, 1);
   if(index == 2) return vec3(1, 1, 0);
   if(index == 3) return vec3(1, 0, 1);
   if(index == 4) return vec3(0.5, 0.5, 1);
   if(index == 5) return vec3(1, 0.5, 0.5);
   if(index == 6) return vec3(0.5, 1, 0.5);
   return vec3(0,1,0);
   
    
}

	   in float v, v2;
	   out vec4 fragColor;
                void main(void) {
		
                    fragColor = vec4(getColor(int(v) + int(v2)), 1.0);
                }
	   ")
				(compile-shader (lambda (source type )
	   								(let ((shader (gl.createShader type)))
										  (gl.shaderSource shader source)
										  (gl.compileShader shader)
										  (unless (gl.getShaderParameter shader gl.COMPILE_STATUS)
											 (println (gl.getShaderInfoLog shader))
											 
											 )
										  shader
										  )
										))

				)
	     (let ((vertex-shader  (compile-shader vertex-source gl.VERTEX_SHADER))
	           (fragment-shader (compile-shader fragment-source gl.FRAGMENT_SHADER))
				  (program (gl.createProgram)))
			 (gl.attachShader program vertex-shader)
			 (gl.attachShader program fragment-shader)
          (gl.linkProgram program)

			 (gl.useProgram program)
			 (println 'ok)
			 
			 (let ((verts (new-f32-array (* 3 2 100)))
					 (buffer (gl.createBuffer))
					 (vert-pos (gl.getAttribLocation program "vert"))
					 )
				(for i 0 (< i 32) (incf i)
					  (let ((index (* i 3 2)))
						 (setnth verts index (/ index (int->float 8)))
						 (setnth verts (+ index 1) 0.0)
						 (setnth verts (+ index 2 ) i)
						 (setnth verts (+ index 3) (/ index (int->float 8)))
						 (setnth verts (+ index 4) 0.5)
						 (setnth verts (+ 5 index) (+ 1 i ))
						 )
					  
					  )
				(gl.bindBuffer gl.ARRAY_BUFFER buffer)
				(gl.bufferData gl.ARRAY_BUFFER verts gl.STATIC_DRAW)

				(gl.vertexAttribPointer vert-pos 3 gl.FLOAT false 0 0)
				(gl.enableVertexAttribArray vert-pos)

				(gl.clearColor 0.0 0.0 0.0 1.0)
				(gl.clear gl.COLOR_BUFFER_BIT)
				(gl.drawArrays gl.TRIANGLE_STRIP 0 28)


				
				)
			 
			 
			 
			 )))
	 ))

;; 2147483647

(defun f32->int-direct (v)
  (getnth (f32-array->u32-array (f32-array v)) 0))

(defun hash-add-i32 (hash v)
   (Math.imul (Math.imul (+ v hash) 41232221) 11232203))

(defun hash-combine (hash-1 hash-2)
   (hash-add-i32 hash-1 hash-2))

(defun hash-add-f32(hash v)
  (let ((v2 (f32->int-direct v)))
	(hash-add-i32 hash v2)))

(defun hash-array (v) 
  (let ((bytes (f32-array->u32-array v))
        (hash 732916321)
		  (len (length bytes)))
	 
	 (for j 0 (< j 2) (incf j)
			(for i 0 (< i len) (incf i)
				  (set hash (xor (+ (getnth bytes i) (Math.imul hash 3213241)) 96382571)   )
				  
				  )
			)
	 
	 hash
	 )
  )

(defun sphere(center radius)
  (let ((sphere (lambda (p)  (- (vec3-length (vec3-sub center p)) radius))))
	 (set sphere.hash (hash-add-f32 (hash-array center) radius))
    (set sphere.bounds sphere)
	(set sphere.center center)
	(set sphere.radius radius)
	(set sphere.type 'sdf-primitive)
    sphere
    )
  )

(defun line (a b r)
   (when (eq r nil)
	  (set r 1.0))

   (let ((l (lambda (p) 
              (let ((ba (vec3-sub b a))
			        (pa (vec3-sub p a)))
					(let ((h (clamp (/ (vec3-dot pa ba) (vec3-dot ba ba)) 0.0 1.0)))
						(- (vec3-length (vec3-sub pa (vec3-scale ba h))) r)
					)
			  ))
         ))
   (set l.sdf-type 'primitive)
   l
   )

)

(defun sdf-gradient (sdf p d)
  (let ((p0 (sdf p)) 
        (px (sdf (vec3-add p (vec3 d 0 0))))
		  (py (sdf (vec3-add p (vec3 0 d 0))))
		  (pz (sdf (vec3-add p (vec3 0 0 d)))))
	 (vec3-scale (vec3 (- px p0) (- py p0) (- pz p0)) (/ 1.0 d)))
  )

(defun sdf-gradient-step (sdf p d)
  (let ((p0 (sdf p)) 
        (px (sdf (vec3-add p (vec3 d 0 0))))
		  (py (sdf (vec3-add p (vec3 0 d 0))))
		  (pz (sdf (vec3-add p (vec3 0 0 d)))))
	 (vec3-add p (vec3-scale 
					  (vec3-normalize (vec3 (- px p0) (- py p0) (- pz p0)))
					  (- p0)))
	 ))


(defun calc-sphere-bounds (sdf)
  (let ((points (list  (vec3 1 0 0) (vec3 0 1 0) (vec3 0 0 1) 
   		              (vec3 -1 0 0) (vec3 0 -1 0) (vec3 0 0 -1))))
    (let ((points2  (map (lambda (pt) (vec3-scale pt (- 10000 (sdf (vec3-scale pt 10000.0))))) points))
			 (l (length points))
			 (max-d 0.0)
			 (p1 nil) (p2 nil)
			 )
													 ;now find the two points farthest from eachother and use that
		(for i 0 (< i l) (incf i)
		     (for j (+ i 1) (< j l) (incf j)
					 (let (( d (vec3-length (vec3-sub (getnth points2 j)  (getnth points2 i)))))
						(when (> d max-d)
						  
						  (set max-d d)
						  (set p1 i)
						  (set p2 j)
						  )
						))
			  )
		(let ((a (getnth points2 p1))
		      (b (getnth points2 p2))
			   (mid (vec3-scale (vec3-add a b) 0.5))
			   (radius (vec3-length (vec3-sub mid a)))
			   )
		  (println mid radius)
        (sphere mid radius))
		
		)))
	
(defun sphere-intersects (a b)
   (< (- (vec3-length (vec3-sub a.center b.center)) a.radius b.radius) 0.0001)
)

(defun sdf-union(&rest sdfs)
  (set sdfs (order-by sdfs (lambda (sdf) (or sdf.hash 1))))
  (let ((f (lambda (p)
				 (let ((m 1000000000.0) (depth 0))
					(for-each f sdfs
								 
								 (set m (min m (f p)))
								 )	 )))
		(depth 0)
		(hash 732132123)
		
		)
	 (for-each f sdfs 
				  (incf depth (or f.depth 1))
				   (set hash (hash-combine hash (or f.hash 1321)))
				  )
	 (set f.sdf-type 'add)
	 (set f.inner sdfs)
	 (set f.bounds (calc-sphere-bounds f))
	 (set f.depth (+ 1 depth))
	 (set f.hash hash)
	 (Object.freeze f)
	 f
	 )
  )
(defun sdf-intersect (a b)
  (let ((f 
			(lambda (p) (max (a p) (b p)))
			 ))
	 (set f.sdf-type 'intersect)
	 (set f.a a)
	 (set f.b b)
	 (set f.depth (+ (or a.depth 1) (or b.depth 1) 1))
	 f
	 )
  )

(defvar infinity-sdf 
   (let ((f (lambda (x) Infinity)))
      (set f.sdf-type 'infinity)
	  (Object.freeze f)
      f))
 
(defun sdf-optimize-intersect (sdf intersect)
   (if (sphere-intersects sdf.bounds intersect.bounds)
      (case sdf.type 
	    ('add

		   (let ((new (where (select sdf.inner (lambda (sub-sdf)
		      (sdf-optimize-intersect sub-sdf intersect)
		    )) (lambda (x) (not (eq infinity-sdf x))))))
			(let ((new-union (apply sdf-union new)))
			   new-union)))

		('primitive 
		  (if (sphere-intersects sdf.bounds intersect.bounds)
		     sdf 
			 infinity-sdf		  
		  )
		)

		   (otherwise sdf)
		
		)
		
	  
	  infinity-sdf
	  )
   
   )

(defun sdf-optimize (sdf)
  (if (eq sdf.sdf-type 'intersect)
    (let ((a (sdf-optimize sdf.a)) 
	      (b (sdf-optimize sdf.b)))
		  ;; the bounds of the intersection does not overlap.
		  (if (not (sphere-intersects a.bounds b.bounds))
		      infinity-sdf
			  (progn 
			  (if (eq a infinity-sdf) b 
			    (if (eq b infinity-sdf) a
			      sdf	
				)
			  )
			  )
		  )
	)
	(if (eq sdf.sdf-type 'add)
	  (progn 
	   ;; if its an add,
	  sdf 
	  )
	  
	  
	  sdf)
  ))

(println (f32-array 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))

(defvar sdf 
  (sdf-union 
	(sphere (vec3 0 -10 0) 3)

	(sdf-union 
	 (sphere (vec3 0 0 0) 1)
	 (sphere (vec3 0 10 0) 1)
	 (sphere (vec3 0 0 -10) 1)  
													 ;(sphere (vec3 0 0 0) 1) 
	 )))

(println (sdf (vec3 0.0 8.0 0.0)))
(println sdf.inner)

(println (equals? (sphere (vec3 1 2 3) 1.0)  (sphere (vec3 1 2 3) 1.0)))

													 ;(defvar sdf 
													 ;    (sdf-intersect sdf (sphere (vec3 0 5.0 0) 9.0)))

													 ;(sdf-optimize sdf)

(let ((p (vec3 5.0 5.0 5.0)))
  (for i 0 (< i 3) (incf i)
		 (let (( g (vec3-normalize (sdf-gradient sdf p 0.001 )))
				 (d (sdf p)))
			(println p "|" g "|" d)
			(set p (vec3-add p (vec3-scale g (- d))))

			))

  )

(calc-sphere-bounds sdf)
(assert-not-eq (hash-add-f32 25 1.001) (hash-add-f32 25 1.01))
(assert (sphere-intersects (sphere (vec3 0 0 0) 1.0) (sphere (vec3 1.5 0 0) 1.0)))
(assert-not (sphere-intersects (sphere (vec3 0 0 0) 1.0) (sphere (vec3 2.5 0 0) 1.0)))

(let ((l1 (line (vec3 0 5 5) (vec3 0 9 9) 0.0))
      (pt (vec3 0 7 7.2)))
	  (println (l1 pt))
)


(let ((test-sdf (sphere (vec3 0 0 0) 1.0))
      (test-intersect (sphere (vec3 3.0 0 0) 1.0)))
	(let ((opt (sdf-optimize-intersect test-sdf test-intersect)))
	   (println opt)
	
	))
