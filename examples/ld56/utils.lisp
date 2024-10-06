(defun find (lst key-selector key)
  (if key
		(block search
		  (foreach elem lst
					  (when (eq (key-selector elem) key)
						 (return-from search elem))))
		(block search
		  (foreach elem lst
					  (when (eq elem key-selector)
						 (return-from search elem))))))
  

(defun filter (lst f key)
  (let ((result (list)))
	 (if key
		  (foreach item lst
					  (when (eq (f item) key)
						 (push result item)))
		  (foreach item lst
					  (when (f item)
						 (push result item))))
	 result))

(defun remove-at(lst i)
  (lst.splice i 1))

(defun remove-if(lst f key)
  (let ((remove-indexes (list)))
	 (if key
		  (dotimes (i (length lst))
			 (when (eq (f (th lst i)) key)
				(push remove-indexes i)))
		  (dotimes (i (length lst))
			 (when (f (th lst i))
				(push remove-indexes i))))
	 (foreach reverse index remove-indexes
				 (remove-at lst index))))
						

(defun remove (lst item)  
  (let ((i (lst.indexOf item)))
	 (remove-at lst i)))

(defun count(lst f)
  (let ((c 0))
	 (foreach x lst
				 (when (f x)
					(incf c)))
	 c))

(defun rectangle (px py cx cy w h)
  (let ((dx (abs (- px cx)))                        ;; Distance along the x-axis
        (dy (abs (- py cy)))                        ;; Distance along the y-axis
        (qx (- dx (/ w 2)))                         ;; Distance outside along the x-axis
        (qy (- dy (/ h 2))))                        ;; Distance outside along the y-axis
    (+ (sqrt (+ (expt (max qx 0) 2) (expt (max qy 0) 2)))  ;; Distance outside the rectangle
       (min (max qx qy) 0))))                        ;; Clamp to 0 when inside the rectangle
