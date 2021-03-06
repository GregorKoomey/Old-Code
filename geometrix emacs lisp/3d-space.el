; objects and code to do matrix math

; requires sos.el
; necessary for 2d-objects.el

; accessors allow for ease of mapping between letter heuristics (used in math books for matrix formulae)
;  and the moderately complex list structure of the sparse matrix. the accessor dict is loaded into an enclosing
;  let at the beginning of all functional methods for ease of use.
; To get data, use (eval a), for example.
; To set data, use (setf a some-data)
; this system is more brittle than it should be because someone might use "set" or "setq", which would overwrite 
;  the value of the accessor path with a literal value; at that point setf would be useless...
; This system will be overhauled on implementation of temp-vars... not sure how the brittle-ness will be dealt with...


(sos-class "3d-space" "geo-metric"
  `((my-matrix nil)
    (temp-matrix nil) ; temp-matrix may become a temp-var when implemented
    (accessors class ; only valid in a method where temp-matrix is defined and expanded
     ,(let ((my-hash (make-hash-table)))
	(puthash 'a '(car (last (car (car (last (car temp-matrix)))))) my-hash)
	(puthash 'b '(car (last (car (car (last (nth 1 temp-matrix)))))) my-hash)
	(puthash 'c '(car (last (car (car (last (nth 2 temp-matrix)))))) my-hash)

	(puthash 'd '(car (last (nth 1 (car (last (car temp-matrix)))))) my-hash)
	(puthash 'e '(car (last (nth 1 (car (last (nth 1 temp-matrix)))))) my-hash)
	(puthash 'f '(car (last (nth 1 (car (last (nth 2 temp-matrix)))))) my-hash)



	(puthash 'g '(car (last (nth 1 (car (last (nth 1 temp-matrix)))))) my-hash)
	(puthash 'h '(car (last (nth 1 (car (last (nth 2 temp-matrix)))))) my-hash)

	my-hash))
    (temp-space-2 class nil ))
  '((init
      (lambda ()
	(set 'my-matrix (compact-sparse-matrix (make-identity-matrix 4)))))
    (re-init
     (lambda ()
       (--- 'init)))
    (get-matrix-list
      (lambda ()
	(expand-sparse-matrix my-matrix 4)))
    (set-to-space-matrix 
      (lambda (a-space-matrix)
	(setq my-matrix (compact-sparse-matrix a-space-matrix))))
    (concat-space 
      (lambda (space-obj &optional dest-space)
	(if (not dest-space)
	    (setq dest-space (! "3d-space")))
	(-- dest-space 'set-to-space-matrix 
	    (sparse-m-times
	     (-- space-obj 'get-matrix-list)
	     (--- 'get-matrix-list)))
	dest-space))
    (translate
      (lambda (x-val y-val &optional dest-space)
	(if (not dest-space)
	    (setq dest-space (! "3d-space")))
	(eval 
	 `(let ,(hash-to-let-list (--- 'get-class-var 'accessors))
	    (let ((my-temp-matrix (expand-sparse-matrix my-matrix 4)))
	      (setq temp-matrix (make-identity-matrix 4))
	      (eval `(setf ,c x-val))
	      (eval `(setf ,f y-val))
	      (setq temp-matrix (compact-sparse-matrix (sparse-m-times my-temp-matrix temp-matrix)))
	      (if (eq self dest-space)
		  (setq my-matrix  temp-matrix)
		(-- dest-space 'set 'my-matrix temp-matrix))
	      setq temp-matrix nil)))
	dest-space))
    (scale
      (lambda (dx-val dy-val &optional dest-space)
	(if (not dest-space)
	    (setq dest-space (! "3d-space")))
	(eval 
	 `(let ,(hash-to-let-list (--- 'get-class-var 'accessors))
	    (let ((my-temp-matrix (expand-sparse-matrix my-matrix 4)))
	      (setq temp-matrix (make-identity-matrix 4))
	      (eval `(setf ,a dx-val))
	      (eval `(setf ,e dy-val))
	      (setq temp-matrix (compact-sparse-matrix (sparse-m-times my-temp-matrix temp-matrix)))
	      (if (eq self dest-space)
		  (setq my-matrix  temp-matrix)
		(-- dest-space 'set 'my-matrix temp-matrix))
	      setq temp-matrix nil)))
	dest-space))
    (rotate
      (lambda (an-angle &optional dest-space)
	(if (not dest-space)
	    (setq dest-space (! "3d-space")))
	(setq an-angle (- 360 an-angle)) ; emacs trig ops assume clockwise angles, where geometrix is counterclock... 
	(eval 
	 `(let ,(hash-to-let-list (--- 'get-class-var 'accessors))
	    (let ((my-temp-matrix (expand-sparse-matrix my-matrix 4))
		  (angle-cos (cos (degrees-to-radians an-angle)))
		  (angle-sin (sin (degrees-to-radians an-angle))))
	      
	      (setq temp-matrix (make-identity-matrix 4))
	      (eval `(setf ,a angle-cos))
	      (eval `(setf ,b angle-sin))
	      (eval `(setf ,d (- 0 angle-sin)))
	      (eval `(setf ,e angle-cos))
	      (setq temp-matrix (compact-sparse-matrix (sparse-m-times my-temp-matrix temp-matrix)))
	      (if (eq self dest-space)
		  (setq my-matrix  temp-matrix)
		(-- dest-space 'set 'my-matrix temp-matrix))
	      setq temp-matrix nil)))
	dest-space))
    (transform
      (lambda (a-point &optional dest-point)
	(if (not dest-point)
	    (setq dest-point (! "3d-point")))
	(eval 
	 `(let ,(hash-to-let-list (--- 'get-class-var 'accessors))
	    (let ((temp-matrix (expand-sparse-matrix my-matrix 4))
		  (old-x (-- a-point 'get-x))
		  (old-y (-- a-point 'get-y)))
	      (-- dest-point 'set-x
		    (clip-float
			(+
			 (* (eval a) old-x)
			 (+
			  (* (eval b) old-y)
			  (eval c)))))
	      (-- dest-point 'set-y
		  (clip-float
		      (+
		       (* (eval d) old-x)
		       (+
			(* (eval e) old-y)
			(eval f)))))
	      (setq temp-matrix nil))))
	dest-point))


))


(let ((temp-space-2 (! "2d-space")))
  (sos-set-class-var "2d-space" 'temp-space-2 temp-space-2))