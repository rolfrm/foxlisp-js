(defun find (lst key-selector key)
  (lst.find (if key (lambda (x) (eq (key-selector x) key)) key-selector)))  

(defun filter (lst f key)
  (lst.filter (if key (lambda (x) (eq (f x) key)) f)))

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
	 (foreach :reverse index remove-indexes
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
  (let ((dx (abs (- px cx)))                       
        (dy (abs (- py cy)))                       
        (qx (- dx (/ w 2)))                        
        (qy (- dy (/ h 2))))                       
    (+ (sqrt (+ (expt (max qx 0) 2) (expt (max qy 0) 2)))
       (min (max qx qy) 0))))
