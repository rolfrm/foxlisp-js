
(defvar svgns "http://www.w3.org/2000/svg")
(defvar model '(svg (rect :width 200 :height 200 :fill blue)))
(println model)
(defvar rootid_counter 0)
;; how to keep track of invalid objects.
;; Invalid objects are those who should no longer be part of the
;; scene graph
;; When iterating the model each node gets a virtual ID
;; corresponding to its position in the three.
;;
'(svg   ; id svg
  (for x (1 2 3) ; svg.for
	(translate :x (bind (* x 200)) ;svg.translate
	 (rect :width 50 :height 50 :fill blue)))) ;svg.translate.rect

;; i could also just set the binding up once, loosing thi 1:1 correspondence.
;;





(defun render-model (model root)
  (let ((root_id (get root 'foxid)))
	 (unless root_id
		(set root_id (rootid_counter.toString))
		(set rootid_counter (+ rootid_counter 1))
		(put root 'foxid root_id)
		)
	 
  (when (list? model)
	 (let ((type (car model)))
		(case type
		  ('svg
			(progn
			  (unless (get root 'svg)
				 (let ((new-elem (document.createElementNS svgns "svg")))
					(root.appendChild new-elem)
					(put root 'svg new-elem)
					))

			  (for-each x (cdr model)
							(render-model x (get root 'svg)))

			  (println 'svg! root)))
		  ('rect
			(progn
			  (unless (get root 'rect)
				 (let ((new_elem (document.createElementNS svgns "rect")))
					(root.appendChild new_elem)
					(put root 'rect new_elem)
					(new_elem.setAttribute "width" "200")
					(new_elem.setAttribute "height" "200")
					(new_elem.setAttribute "fill" "blue")
					))
				 
			  
			  (println "rect!")))
		  

		  )))))
(defmacro for (varsym start stop increment &rest body)
  `(let ((,varsym ,start))
	  (loop ,stop
		,@body
		,increment
	  )
	  ))

(defmacro incr(sym incr_value)
  (unless incr_value
	 (set incr_value 1))
  `(set ,sym (+ ,sym ,incr_value))
  )

(defmacro push (location value)
  `(let ((loc ,location))
	  (loc.push ,value))
  )

(let ((lst (list)))
  (for x 0 (< x 10) (incr x) (push lst x))
  (println lst)
  )


(defvar current-element nil)
(defvar elem-id 0)
(defvar prev-id "0")

(defmacro in-element(elem &rest body)
  `(let ((e2 current-element) (prev-id2 (+ prev-id "." (incf elem-id))))
	  (let ((current-element (get-element ,elem e2 prev-id2))
			  (prev-id prev-id2)
			  (elem-id 0))
		 (progn ,@body)
		 )
	  ))

(defvar has-document 1)
(defvar svgelements (makehashmap))
(defun get-element (name parent id)
  (if has-document
		(progn
		  (unless parent
			 (set parent document.body)
			 )
		  
		  (println 'reuse? id (svgelements.has id))
		  (if (svgelements.has id)
				(progn
				  (svgelements.get id))
				(let ((element (document.createElementNS svgns name.value)))
				  (println 'create-element-on parent)
				  (parent.appendChild element)
				  (svgelements.set id element)
				  element
				  )))
		(list name)
  
  ))

(defun set-value (item key value)
  (println 'set-value item (cdr key.value) value)
  (item.setAttribute (cdr key.value) (if (symbol? value) value.value value)))

(defun set-text-content(item value)
  (set item.textContent value)
  )

(defun get-body-items(body)
  (let ((body-items (list)))
	 (for i 0 (< i (length body)) (incr i)
			(let ((item (nth body i)))
			  (unless (list? item)
				 (push body-items `(set-value current-element ',item ,(nth body (+ 1 i))))
				 (incr i))))
	 (println 'body-items: body-items)
	 body-items))

(defun get-body-elements(body)
  (let ((body-elements (list)))
	 (for i 0 (< i (length body)) (incr i)
			(let ((item (nth body i)))
			  (println 'item? item)
			  (if (list? item)
					(progn
					  (if (ismacro (car item))
				 		 (push body-elements item)
						 (let ((new-elements (get-body-elements (cdr item))))
							(println 'new-elements: new-elements item)
							(for-each item2 new-elements
										 (push body-elements item2)))
						 ))
					  (incr i)
					))
			)
	 (println 'body-elements: body-elements)
	 body-elements))

(defmacro rect (&rest body)
  `(in-element 'rect
					(set-value current-element ':transform "translate(-0.5,-0.5)")
					(set-value current-element ':width 1)
					(set-value current-element ':height 1)
					,@(get-body-items body)
					,@(get-body-elements body)
					))

(defmacro scope-for(arg iter &rest body)
  `(for-each iterator ,iter
				 
				 (let ((,arg iterator)) ,@body)))



(defmacro svg (&rest body)
  `(let ((elem-id 0))
	  (in-element 'svg
					  ,@(get-body-items body)
					  
					  ,@(get-body-elements body))))

(defmacro text (text &rest body)
  `(in-element 'text
					,@(get-body-items body)
					(set-text-content current-element ,text)
					
					,@(get-body-elements body)))


(defmacro translate (x y &rest body)
  `(in-element 'g
					(set-value current-element ':transform (+ "translate(" ,x "," ,y ")")
								  ,@(get-body-elements body) )))

(defmacro rotate(angle &rest body)
  `(in-element 'g
					(set-value current-element ':transform (+ "rotate(" ,angle ",0,0)"))
					,@(get-body-elements body) ))
(defmacro scale(x y &rest body)
  `(in-element 'g
					(set-value current-element ':transform (+ "scale(" ,x "," ,y ")"))
								  ,@(get-body-elements body) ))

(println (ismacro 'scale))

(when 0
  (svg :width 900 :height 500
		 (translate 50 250
						(scope-for x '( (11 purple) (10 indigo) (9 black) (8 darkgray) (7 gray) (6 lightgreen) (5 lightblue) (4 pink)  (3 green) (2 blue) (1 red) )
									  
									  (translate (* 50 (car x)) 0
													 (rotate (* 20 (- (car x) 1))
																
																(scale (* (car x) 50) 50
																		 (rect
																		  :fill (cadr x)
																		  )
																		 
																		 )
																(asd (text  "hello world")
																 )
																))))))

(defun onclick(x y)
  (println (list "click" (x.target.getAttribute "fill") (x.target.getAttribute "xs") (x.target.getAttribute "ys")  y)))

(when 0
  (svg :width 500 :height 500
		 (scale 10 10
				  (translate 1 1
				  (scope-for x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
								 (scope-for y '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
												(translate (* x 2)  (* y 2)
															  (rect :fill 'blue :xs x :ys y :onClick "onclick(10, 20)")))


				  )))))


;(svg :width 900 :height 500
;	  (scale 50 50
;				(rect :fill 'lightblue)))
