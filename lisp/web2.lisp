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
(defun vec3-x (v) (elt v 0))
(defun vec3-y (v) (elt v 1))
(defun vec3-z (v) (elt v 2))

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

(defun vec3-normalize (v) (vec3* v (/ 1.0 (vec3-length v))))


(defun vec3-(a b)
  (vec3-combine a b (lambda (x y) (- x y)))
  )
(defun vec3+(a b)
  (vec3-combine a b (lambda (x y) (+ x y)))
  )

(defun vec3*(a s)
  (vec3-apply a (lambda (x) (* x s)))
  )

(defun vec3-dot(a b)
   (+ (* (vec3-x a) (vec3-x b))
      (* (vec3-y a) (vec3-y b))
	  (* (vec3-z a) (vec3-z b)))
)

(defun vec3-abs(a) (vec3-apply a abs))

(defun vec3-max(a n)
   (if (number? n)
     (vec3-apply a (lambda (v) (max v n)))
	 (vec3-combine a n (lambda (v v2) (max v v2))))
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
  (- (vec3-length (vec3- p c)) r))

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
				 (let ((new-p (vec3+ (vec3* d c) p)))
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
  (let ((sphere (lambda (p)  (- (vec3-length (vec3- center p)) radius))))
	 (set sphere.hash (hash-add-f32 (hash-array center) radius))
    (set sphere.bounds sphere)
	(set sphere.center center)
	(set sphere.radius radius)
	(set sphere.sdf-type 'sphere)
    sphere
    )
  )

(defun sd-box (p b)
  (let ((q (vec3- (vec3-abs p) b)))
    (+ (vec3-length (vec3-max q 0.0))
       (min (max (elt q 0) (max (elt q 1) (elt q 2))) 0.0))))

(defun aabb (min-axes max-axes)
	(let ((center (vec3* (vec3+ min-axes max-axes) 0.5))
	      (radius (vec3- max-axes center))
	    )
		(println center radius)
		(let ((l  (lambda (p) (sd-box (vec3- p center) radius))))
         (set l.sdf-type 'primitive)
		 (set l.bounds (sphere center (apply max radius)))
		l
		)
	)
)

(defun cube-sdf (offset size)
   (let ((l (lambda (p) 
      (let ((p0 (vec3-abs (vec3- p offset))))
        (- (max (vec3-x p0) (vec3-y p0) (vec3-z p0)) size)

	  )
   
   )))
   (set l.sdf-type 'primitive)
   (set l.bounds (sphere offset (* size 1.72)))
   (println l.bounds)
      l
   )
)


(defun line (a b r)
   (when (eq r nil)
	  (set r 0.0))

   (let ((l (lambda (p) 
              (let ((ba (vec3- b a))
			        (pa (vec3- p a)))
					(let ((h (clamp (/ (vec3-dot pa ba) (vec3-dot ba ba)) 0.0 1.0)))
						(- (vec3-length (vec3- pa (vec3* ba h))) r)
					)
			  ))
         )(bounds (sphere (vec3* (vec3+ a b) 0.5) 
		     (+ r (vec3* (vec3-length (vec3- a b)) 0.5))))
		 
		 )
   (set l.sdf-type 'primitive)
   (set l.bounds bounds)
   l
   )

)

(defun sdf-gradient (sdf p d)
  (let ((p0 (sdf p)) 
        (px (sdf (vec3+ p (vec3 d 0 0))))
		  (py (sdf (vec3+ p (vec3 0 d 0))))
		  (pz (sdf (vec3+ p (vec3 0 0 d)))))
	 (vec3* (vec3 (- px p0) (- py p0) (- pz p0)) (/ 1.0 d)))
  )

(defun sdf-gradient-step (sdf p d)
  (let ((p0 (sdf p)) 
        (px (sdf (vec3+ p (vec3 d 0 0))))
		  (py (sdf (vec3+ p (vec3 0 d 0))))
		  (pz (sdf (vec3+ p (vec3 0 0 d)))))
	 (vec3+ p (vec3* 
					  (vec3-normalize (vec3 (- px p0) (- py p0) (- pz p0)))
					  (- p0)))
	 ))


(defun calc-sphere-bounds (sdf)
  (let ((points (list  (vec3 1 0 0) (vec3 0 1 0) (vec3 0 0 1) 
   		              (vec3 -1 0 0) (vec3 0 -1 0) (vec3 0 0 -1))))
    (let ((points2  (map (lambda (pt) (vec3* pt (- 10000 (sdf (vec3* pt 10000.0))))) points))
			 (l (length points))
			 (max-d 0.0)
			 (p1 nil) (p2 nil)
			 )
													 ;now find the two points farthest from eachother and use that
		(for i 0 (< i l) (incf i)
		     (for j (+ i 1) (< j l) (incf j)
					 (let (( d (vec3-length (vec3- (getnth points2 j)  (getnth points2 i)))))
						(when (> d max-d)
						  
						  (set max-d d)
						  (set p1 i)
						  (set p2 j)
						  )
						))
			  )
		(let ((a (getnth points2 p1))
		      (b (getnth points2 p2))
			   (mid (vec3* (vec3+ a b) 0.5))
			   (radius (vec3-length (vec3- mid a)))
			   )
		  (println mid radius)
        (sphere mid radius))
		
		)))
	
(defun sphere-intersects (a b)
   (< (- (vec3-length (vec3- a.center b.center)) a.radius b.radius) 0.0001)
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
(defun sdf-subtact (a b)
  (let ((f 
			(lambda (p) (max (a p) (- (b p))))
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
	  (set f.bounds (sphere (vec3 0 0 0) 10000000.0))
	  (Object.freeze f)
      f))

(defun aabb-bounds-2-sphere (a b)
   
   (let ((min-pt (vec3-combine a.center b.center (lambda (a2 b2) (min (- a2 a.radius) (- b2 b.radius)))))
         (max-pt (vec3-combine a.center b.center (lambda (a2 b2) (max (+ a2 a.radius) (+ b2 b.radius))))))
	(println min-pt max-pt)

   )
)

(defun sdf-intersects2 (a b offset size)
	"(let ((a-d (a offset)) (b-d (b offset)))
	  (cond 
	    ;; collision?
	    ((and (< a-d 0.0001) (< b-d 0.0001)) t)
		;; both outside bounds
		((or (> a-d (* sqrt2 size)) (> b-d (* sqrt2 size))) nil)
		;; iterate more
		(t 
		   
		)
	
	)
 )")
 
(defun sdf-intersects (a b bounds)
   (let ((offset bounds.center) (size bounds.radius))
   
   
   )
   (aabb-bounds-2-sphere a.bounds b.bounds)
)

(defun lispify (sdf)
   (println sdf sdf.sdf-type)
   (case sdf.sdf-type 
      ('add 
	    (list 'add (select sdf.inner lispify))
	  )
	  ('sphere (list 'sphere sdf.center sdf.radius))
	  (otherwise sdf.sdf-type)
   
   )

)

(defun sdf-optimize-intersect (sdf intersect)
   (if (sphere-intersects sdf.bounds intersect.bounds)
      (case sdf.sdf-type 
	    ('add
		
		   (let ((new (where (select sdf.inner (lambda (sub-sdf)
		      (sdf-optimize-intersect sub-sdf intersect)
		    )) (lambda (x) (not (eq infinity-sdf x))))))
			(case (length new)
			  (0  infinity-sdf)
			  (1 (car new))
			  (otherwise 
			    (if (equals? new sdf.inner)
					sdf
					(apply sdf-union new)	
				)
			  )
			)))
		('sphere 
		  (if (sphere-intersects sdf intersect.bounds)
		     sdf 
			 infinity-sdf		  
		  )
		)

		(otherwise 
		  (if (sphere-intersects sdf.bounds intersect.bounds)
		     (if (sdf-intersects sdf intersect intersect.bounds)
			 	sdf 
				infinity-sdf		  	
			 )
		     
			 infinity-sdf		  
		  )
		)
		
		)
		
	  
	  infinity-sdf
	  )
   
   )

(defun sdf-optimize (sdf)
  (if (eq sdf.sdf-type 'intersect)
    (let ((a (sdf-optimize sdf.a)) 
	      (b (sdf-optimize sdf.b)))
		  ;; the bounds of the intersection does not overlap.
		  (if (not (sphere-intersects a.bounds b))
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

(defun trace-ray (sdf p d)
  (let ((go 1))
  (for i 0 (and go (< i 200)) (incf i)

     (let ((dist (sdf p)))
	    (when (< dist 0.01)
		   (set go 0)
		)
		(when (> dist 400.0)
		   (set go 0)
		)
	    ;(println dist p d)
	     (set p (vec3+ p (vec3* d dist)))
	 )
  )
  p
))
(defun write-color (array offset color) 
  (set color (vec3-apply color (lambda (x) (floor (* 255 (clamp x 0 1.0))))))
  (setnth array offset (vec3-x color))
  (setnth array (+ offset 1) (vec3-y color))
  (setnth array (+ offset 2) (vec3-z color))

)

(defun sdf-color(sdf color)
   (set sdf.color color)
   sdf
)
(defun color-at (sdf p)
   (block hej
   (let ((d (if sdf.color (sdf p) 10000)))
   ;(println 'sdf.color: sdf.color sdf.sdf-type)
     (if (< d 0.01)
	    sdf.color
		(case sdf.sdf-type 
		   ('add 
		     (for-each item sdf.inner 
			   (let ((c (color-at item p)))
			     (when c (return-from hej c))
			      
			   )
			 
			 )
		   
		   )
		   (otherwise nil)
		
		)
		)
   
   )))

(defvar sdf
(sdf-union 
  (sdf-union 
	(sphere (vec3 0 -10 0) 1)

	(sdf-union 
	 (sphere (vec3 0 0 0) 1)
	 (sphere (vec3 0 10 0) 1)
	 (sphere (vec3 0 0 -10) 1)  
	 
													 ;(sphere (vec3 0 0 0) 1) 
	 )))

	 )


(defvar sdf 
  (sdf-union 
	(sphere (vec3 -5 1 0) 1)
	(sdf-color 
    (sphere (vec3 -0 -103 0) 100) (vec3 0 0 1)  )
	(sdf-color 
	 (sdf-union 
	 (sdf-color 
	 (sphere (vec3 3 3 3) 1) (vec3 0 1 0))
	 (sphere (vec3 0 5 0) 1.3)
	 (sphere (vec3 0 3 5) 1.5)
	 
	 (aabb (vec3 -3 1 -3) (vec3 -2 2 -2))
	 (sdf-color 
	    (line (vec3 -10 -1 0) (vec3 10 -1 0) 0.5) (vec3 1 0 0) )
	(sdf-color 
	    (line (vec3 -10 -1 0) (vec3 0 -1 10) 0.5) (vec3 1 0 0) )
		(sdf-color 
	    (line  (vec3 0 -1 10) (vec3 10 -1 0) 0.5) (vec3 1 0 0) )
	 ;(line (vec3 0 0 0) (vec3 0 10 0) 0.05)
	 ;(line (vec3 0 0 0) (vec3 0 0 100) 0.05)
	 
													 ;(sphere (vec3 0 0 0) 1) 
	 ) (vec3 1 0 0)
	 
	 )))
(defun sdf-skip (&rest args)
   infinity-sdf

)
(defvar sdf 

 (sdf-union 
 	 (aabb (vec3 -15 -8 -15) (vec3 15 -6 15))
	 
  (sdf-union 
     (sphere (vec3 0 0 0) 5.0)
	(sdf-union (line (vec3 0 0 -50) (vec3 0 0 50) 2)
	   (line (vec3 -50 0 0) (vec3 50 0 0) 2)
	   (line (vec3 0 -50 0) (vec3 0 50 0) 2)
	   )
	 
	 )))

(println (sdf (vec3 0.0 8.0 0.0)))
(println sdf.inner)

(println (equals? (sphere (vec3 1 2 3) 1.0)  (sphere (vec3 1 2 3) 1.0)))

													 ;(defvar sdf 
													 ;    (sdf-subtact sdf (sphere (vec3 0 5.0 0) 9.0)))

													 ;(sdf-optimize sdf)

(let ((p (vec3 5.0 5.0 5.0)))
  (for i 0 (< i 3) (incf i)
		 (let (( g (vec3-normalize (sdf-gradient sdf p 0.001 )))
				 (d (sdf p)))
			(println p "|" g "|" d)
			(set p (vec3+ p (vec3* g (- d))))

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


(let ((test-sdf (sdf-union
   (sphere (vec3 0 0 0) 1.0)
   (sphere (vec3 -0.5 0 0) 1.0)
   
   ))
      (test-intersect (sphere (vec3 1.0 0 0) 2.0)))


	(let ((opt (sdf-optimize-intersect test-sdf test-intersect)))
	   (println 'result: opt)
	   (sdf-intersects test-sdf test-intersect)
	
	))

(defvar cells (list (vec3 -1 -1 -1) (vec3 1 -1 -1) ))

(defun render-world (sdf offset size subdiv)
   ;; offset - center of box
   ;; size width
   ;; subdiv - number of subdivisions
   (let ((intersect (cube-sdf offset size)))
     
     (let ((sdf2 (sdf-optimize-intersect sdf intersect)))
		(set sdf sdf2)
	 )
   )
   (println (lispify sdf))
   
   (let ((size2 (* size 0.5)))
      (if subdiv 
         (for-each cell cells 
	      (render-world sdf (vec3+ offset (vec3* cell size2)) size2 (- subdiv 1))
	     )
		 ;(println 'box: offset size)

	  )
   
   )

)


(println (vec3-max (vec3 1 4 6) (vec3 9 4 2)))

(defvar d0 (vec3-normalize (vec3 1.0 -1.0 -1.0)))
(defvar right (vec3 0.5 0 0.5))
(defvar up (vec3 0.0 1.0 0.0))
(defvar scale-x 0.5)
(defvar scale-y 0.5)
(defvar sun-loc (vec3 0 100 30))
(println d0)

(defvar png (%js "require('pngjs').PNG"))
(defvar fs (%js "require('fs')"))
(defvar p1 (%js "new png({width: 128, height: 128})" ))
(when t
(for i 0 (< i 128) (incf i)
   (for j 0 (< j 128) (incf j)
      (let ((color nil) (grad nil))
       (let ((index (* (+ (* 128 i) j) 4)))
            (let ((px (* scale-x (- j 64)))
			       (py (* scale-y (- (- i 64)))))
				(let ((p0 (vec3+ (vec3* up py) (vec3* right px))))
				   (let ((pstart (vec3- p0 (vec3* d0 100.0))))
				     (let ((loc (trace-ray sdf pstart d0)))
					   (when (< (sdf loc) 0.05) 
					     (let ((sund (vec3-normalize (vec3- sun-loc loc )))
						       )
							   (set grad (sdf-gradient sdf loc 0.001))
						    (let ((loc2 (trace-ray sdf (vec3+ loc (vec3* sund 0.1)) sund))
							      (diffuse (max 0.0 (vec3-dot grad sund)))
							)
							(set color (or (color-at sdf loc) (vec3 1 1 1)))
							(let ((shadow (if (> (vec3-length (vec3- loc loc2)) 10.0)
								1.0
								0.7
							)))
							
							(println diffuse)
							(set color (vec3* (vec3* color (+ 0.5 (* diffuse 0.5))) (+ 0.1 shadow)))
							(println ">> " color)
						 
							)
							
						 ))
					     
					   ) 
					 )
				   
				   )

				)
			)

			(when color 
			(write-color p1.data index color)
			
			)


	   		(setnth p1.data (+ index 3) 255)
	   ))
       
   )
   
))


(defvar fp (fs.createWriteStream "a.png"))
(let ((packed (p1.pack)))
   (packed.pipe fp))


(let ((test-aabb (aabb (vec3 0 0 0) (vec3 1 1 1))))
   (println 'asd (test-aabb (vec3 2.5 2.5 2.5)))
)
(println (color-at sdf (vec3 3 3 3)))

(println d0)
(println 'render-world)
(let ((sdf2 (sdf-union (sphere (vec3 1 0 0) 0.2) 
                       (sphere (vec3 0 0 0) 0.2)
					   (sphere (vec3 -1 0 0) 0.2)
					)))
	(println (render-world sdf2 (vec3 0 0 0) 4 3))
)

(println 'all-done)