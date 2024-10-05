(defun find (lst key-selector key)
  (block search
	 (foreach elem lst
				 (when (eq (key-selector elem) key)
					(return-from search elem)))))

(defun filter (lst f key)
  (if key
		(lst.filter (lambda (x) (eq (f x) key)))
		(lst.filter f)))
