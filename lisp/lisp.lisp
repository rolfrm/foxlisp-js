(loadfile "lisp/lisp0.lisp")

(defun prefix-symbols (prefix code)
  (let ((any-new nil)
        (result code))
    
    (dotimes (i (length code))
      (let ((x (nth code i)))
        (if (symbol? x)
          (let ((existing-sym (lookupsym (concat (symbol-name prefix) (symbol-name x)))))
            (when existing-sym
              (set x existing-sym)))
          (when (list? x)
            (set x (prefix-symbols prefix x))))
        (unless (eq x (nth code i))
          (when (eq code result)
            (set result (apply list code)))
          (setnth result i x ))))
    result))

(defmacro with-prefix (prefix &rest body)
  `(progn ,@(prefix-symbols prefix body)))

(defun $-impl (context index)
  (let ((new-context (take context (+ 1 index)))
		  (existing (nth context index)))
	 (setnth new-context index (concat (cdr existing) (skip context (+ 1 index))))
	 new-context))

(defun !-impl (context index)
  (let ((pre (take context index))
		  (post (skip context (+ 1 index))))
	 (println (concat pre (list post)))))

(defvar super-macros (makehashmap))
(hashmap-set super-macros '$ $-impl)
(hashmap-set super-macros '! !-impl)
(hashmap-set super-macros ', !-impl)

;; this code scans for and applies super-macros
(defun reader-replacer (code)
  (if (list? code)
		(let ((orig-code code))
		  (dotimes (i (length code))
			 (let ((x (th code i)))
				(when (list? x)
				  (if (symbol? (car x))
						(let ((sw (hashmap-get super-macros (car x))))
						  
						  (if sw
								(let ((result (sw code i)))
								  (set code result)
								  (set i 0))
								(progn
								  (when (eq code orig-code)
									 (set code (apply list code)))
					 			  (setnth code i (reader-replacer x))
								  )))
						(let ((new-v (reader-replacer x)))
						  (when (eq code orig-code)
							 (set code (apply list code)))
						  (setnth code i new-v))))
				(when (symbol? x)
				  (let ((sw (hashmap-get super-macros x)))
					 (when (eq code orig-code)
						(set code (apply list code)))
						
					 (when sw
						(set code (sw code i))
						(set i 0)))
				  (when (is-constant x)
					 
					 (when (eq code orig-code)
						(set code (apply list code)))
					 (let ((constant (get-constant x)))
						(setnth code i (if (list? constant)
												 (list 'quote (get-constant x))
												 constant)))
					 (set i 0))
				  )))
		  code)
		(if (and (symbol? code) (is-constant code))
			 (get-constant code)
			 code)))

(set lisp_reader reader-replacer)

(defun mul-builder (args2)
  (if (> (length args2) 1)
		(concat (list "" (car args2) " * ") (mul-builder (cdr args2)) (list ""))
		(if (eq (length args2) 0)
			 (list 1)
			 args2)))

(defmacro * (&rest args2)
  `(%js ,@(mul-builder args2)))

(defun div-builder (args2)
  (if (> (length args2) 1)
		(concat (list "" (car args2) " / (") (mul-builder (cdr args2)) (list ")"))
		(if (eq (length args2) 0)
			 (list 1)
			 args2)))
;; (/ 4) -> 0.25
;; (/ 1 4) -> 0.25
;; (/ 10 5 2) -> 1
;(defmacro / (&rest args2)
;  (if (eq (length args2) 1)
;		`(%js "(1.0 / " ,(car args2) ")")
;		`(%js ,@(div-builder args2))))

(defun add-builder (args2)
  (if (> (length args2) 1)
		(concat (list "(" (car args2) " + ") (add-builder (cdr args2)) (list ")"))
		(if (eq 0 (length args2))
			 (list 0)
			 args2)))

(defmacro + (&rest args2)
  `(%js ,@(add-builder args2)))

(defun sub-builder (args2)
  (if (> (length args2) 1)
		(concat (list "(" (car args2) " - ") (sub-builder (cdr args2)) (list ")"))
		(if (eq 0 (length args2))
			 (list 0)
			 args2)))

(defmacro - (&rest args2)
  (if (eq (length args2) 1)
		`(%js "(-" ,(car args2) ")")
		`(%js ,@(sub-builder args2))))

(defmacro < (&rest args2)
  (if (eq (length args2) 2)
		`(%js ,(car args2) " < " ,(cadr args2))
		(raise "!!!")))

(defmacro % (x y)
  `(%js "( " ,x " % " ,y " )"))
