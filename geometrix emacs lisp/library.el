; useful functions


; Sparse Matrix functions
; adapted from Winston and Horn LISP, p.491-493

(defun sparse-scale-v (scale v)
  (if (zerop scale) nil
    (if (not v) nil
      (let ((fv (car v)))
	(cons (list (car fv)
		    (* scale (car (cdr fv))))
	      (sparse-scale-v scale (cdr v)))))))


(defun sparse-dot-product (a b)
  (if (or (not a) (not b)) 0
    (let ((fa (car a)) (fb (car b)))
      (cond ((< (car fa) (car fb))
	     (sparse-dot-product (cdr a) b))
	    ((> (car fa) (car fb))
	     (sparse-dot-product a (cdr b)))
	    (t (+ (* (car (cdr fa)) (car (cdr fb)))
		     (sparse-dot-product (cdr a) (cdr b))))))))

(defun sparse-v-plus (a b)
  (cond ((not a) b)
	((not b) a)
	(t (let ((fa (car a))(fb (car b)))
	     (cond ((< (car fa) (car fb))
		    (cons fa
			  (sparse-v-plus (cdr a) b)))
		   ((> (car fa) (car fb))
		    (cons fb
			  (sparse-v-plus a (cdr b))))
		   (t (cons (list (car fa)
				  (+ (car (cdr fa))
				     (car (cdr fb))))
			    (sparse-v-plus (cdr a) (cdr b)))))))))


(defun sparse-m-times-v (m v)
  (if (or (not v) (not m)) nil
    (let ((fv (car v))(fm (car m)))
      (cond ((< (car fv) (car fm))
	     (sparse-m-times-v m (cdr v)))
	    ((> (car fv) (car fm))
	     (sparse-m-times-v (cdr m) v))
	    (t (sparse-v-plus
		(sparse-scale-v (car (cdr fv)) (car (cdr fm)))
		(sparse-m-times-v (cdr m) (cdr v))))))))


(defun sparse-m-times (ma mb)
  (if (not mb) nil
    (let ((fb (car mb)))
      (cons (list (car fb)
		  (sparse-m-times-v ma (car (cdr fb))))
	    (sparse-m-times ma (cdr mb))))))
				  
; test matrix code from winston and horn LISP (to illustrate format expected)
;(sparse-scale-v 2 '((1 1.2) (3 3.4) (6 -6.7)))
;(sparse-dot-product '((1 2) (3 3) (6 4)) '((1 1) (6 3)))
;(sparse-v-plus '((1 2) (3 3) (6 4)) '((1 1) (6 3)))
;(sparse-m-times '((1 ((2 2))) (2 ((1 1))) (3 ((3 1))))
;		'((1 ((2 3))) (2 ((3 3))) (3 ((1 4)))))

    
; my sparse matrix utility functions

(defun copy-sparse-matrix (a-matrix)
  (copy-tree a-matrix))

(defun make-identity-matrix (an-int)
  (let ((new-matrix nil)(column an-int) row-list row value)
    (while (> column 0)
      (setq row an-int)
      (setq row-list nil)
      (while (> row 0)
	(if (= row column)
	    (setq value 1)
	  (setq value 0))
	(setq row-list (cons (list row value) row-list))
	(setq row (- row 1)))
      (setq new-matrix (cons (list column row-list) new-matrix))
      (setq column (- column 1)))
    new-matrix))

(defun make-blank-matrix (an-int)
  (let ((new-matrix nil)(column an-int) row-list row value)
    (while (> column 0)
      (setq row an-int)
      (setq row-list nil)
      (while (> row 0)
	(setq value 0)
	(setq row-list (cons (list row value) row-list))
	(setq row (- row 1)))
      (setq new-matrix (cons (list column row-list) new-matrix))
      (setq column (- column 1)))
    new-matrix))



(defun compact-sparse-matrix (a-matrix)
  (let (new-matrix old-column-list new-column-list new-column-data-list column-data-list column-value old-row-list new-entry)
    (while a-matrix
      (setq old-column-list (car a-matrix))
      (setq new-column-list (list (car old-column-list)))
      (setq new-column-data-list '()) 
      (setq column-data-list (car (cdr old-column-list)))
      (while column-data-list
	(setq old-row-list (car column-data-list))
	(if (not (zerop (car (cdr old-row-list))))
	    (setq new-column-data-list (append new-column-data-list (list (copy-tree old-row-list))))
;	  (message "zero value in matrix element")
	  )
	(setq column-data-list (cdr column-data-list)))
      (setq new-column-list (append new-column-list (list new-column-data-list)))
      ;(print `("new-column-list" ,new-column-list))
      (setq new-matrix (append new-matrix (list new-column-list)))
      (setq a-matrix (cdr a-matrix)))
    new-matrix))

(defun expand-sparse-matrix (a-matrix an-int)
  (let ((new-matrix (make-blank-matrix an-int))
	temp-matrix
	current-column current-column-index current-column-data
	current-cell current-cell-index current-cell-value
	new-column new-column-data new-cell)
    (while a-matrix
      (setq current-column (car a-matrix))
      (setq current-column-index (car current-column))
      (setq current-column-data (car (cdr current-column)))
      (while current-column-data
	(setq current-cell (car current-column-data))
	(setq current-cell-index (car current-cell))
	(setq current-cell-value (car (cdr current-cell)))
	; find corresponding new column (must exist in newly allocated matrix-- error check?)
	(setq temp-matrix new-matrix)
	(setq new-column nil)
	(while (and temp-matrix (not new-column))
	  (if (= (car (car temp-matrix)) current-column-index)
		(setq new-column (car temp-matrix)))
	  (setq temp-matrix (cdr temp-matrix)))
	(if new-column
	    (progn
	      (setq new-column-data (car (cdr new-column)))
	      (setq new-cell nil)
	      (while (and new-column-data (not new-cell))
		(if (= (car (car new-column-data)) current-cell-index)
		    (setq new-cell (car new-column-data)))
		(setq new-column-data (cdr new-column-data)))
	      (if new-cell
		  (setf (car (cdr new-cell)) current-cell-value)
		(message "expand-sparse-matrix error: no new cell")))
	  (message "expand-sparse-matrix error: no new column"))
	(setq current-column-data (cdr current-column-data)))
      (setq a-matrix (cdr a-matrix)))
    new-matrix))




; sample data: 4x4 identity matrix
; ((1 ((1 1) (2 0) (3 0) (4 0))) 
;  (2 ((1 0) (2 1) (3 0) (4 0))) 
;  (3 ((1 0) (2 0) (3 1) (4 0))) 
;  (4 ((1 0) (2 0) (3 0) (4 1))))



;radian/degree conversion

(defun degrees-to-radians (num)
  (/ num 57.295779513082))

(defun radians-to-degrees (num)
  (* num 57.295779513082))


; taming floating point values
(defun clip-float (float-value &optional scale-int) 
  (let ((scale-value nil)
	(integer-part 
	 (if (= float-value float-value)
	     (truncate float-value)
	   (progn
	     (message "ERROR clip-float: value not a number")
	     0.0)))
	(remainder-part 
	 (if (= float-value float-value)
	     (- float-value (truncate float-value))
	   (progn
	     (message "ERROR clip-float: value not a number")
	     0.0))))
    (setq scale-value (expt 10 7))
    (if scale-int 
	(setq scale-value 
	      (if (< scale-int 8) 
		  (expt 10 scale-int) 
		(expt 10 7))))
       (let ((temp-value (* scale-value remainder-part)))
	 (setq temp-value (float (round temp-value)))
	 (+ integer-part (/ temp-value scale-value)))))

; geometrical data generation

