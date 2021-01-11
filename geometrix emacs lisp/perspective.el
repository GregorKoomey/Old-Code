; classes for objects that implement rendering of 3-dimensional space using linear perspective


; "*" at end of symbol refers to usage of 3d-points by things that act as do their 
;  similarly named 2d cousins.



(sos-class "vertex-element*" "vertex-element"
  nil
  '((init
    (lambda ()
      (if (not destination-point)
	  (setq destination-point (! "3d-point")))))
    (set-xyz*
     (lambda (x-val y-val z-val)
       (-- destination-point 'set-xyz* x-val y-val z-val)))
    (get-x*
     (lambda ()
       (-- destination-point 'get-x*)))
    (get-y*
     (lambda ()
       (-- destination-point 'get-y*)))
    (get-z*
     (lambda ()
       (-- destination-point 'get-z*)))
    (3d-vertex-equiv?
     (lambda (element)
       (if element
	   (if (-- element 'is-a? "3d-vertex-element")
	       (if (and 
		    (and
		     (= (--- 'get-x*) (-- element 'get-x*))
		     (= (--- 'get-y*) (-- element 'get-y*)))
		    (= (--- 'get-z*) (-- element 'get-z*)))
		   t
		 nil)
	     nil)
	 nil)))))


(sos-class "move-to*" "vertex-element*"
  nil
  '((get-text
     (lambda (&optional a-matrix)
       (concat (-- destination-point 'get-text (if a-matrix a-matrix)) "m" "\n")))))


(sos-class "line-to*" "vertex-element*"
  '((corner nil))
  '((get-text
     (lambda (&optional a-matrix)
       (concat 
	(-- destination-point 'get-text (if a-matrix a-matrix)) 
	(if corner
	    (eval "l")
	  (eval "L"))
	"\n")))))


(sos-class "curve-to*" "vertex-element*"
  '((control-point-1 nil)(control-point-2 nil)(corner nil))
  '((init
     (lambda()
       (if (not control-point-1)
		(setq control-point-1 (! "3d-point")))
       (if (not control-point-2)
		(setq control-point-2 (! "3d-point")))
       (if (not destination-point)
		(setq destination-point (! "3d-point")))))
    (get-text
     (lambda (&optional a-matrix)
       (concat 
	(-- control-point-1 'get-text (if a-matrix a-matrix)) 
	(-- control-point-2 'get-text (if a-matrix a-matrix)) 
	(-- destination-point 'get-text (if a-matrix a-matrix)) 
	(if corner
	    (eval "c")
	  (eval "C"))
	"\n")))
    (set-x1
     (lambda (value)
       (-- control-point-1 'set-x value)))
    (set-y1
     (lambda (value)
       (-- control-point-1 'set-y value)))

    (set-x2
     (lambda (value)
       (-- control-point-2 'set-x value)))
    (set-y2
     (lambda (value)
       (-- control-point-2 'set-y value)))
    (set-values 
     (lambda (xval1 yval1 xval2 yval2 xval3 yval3)
       (-- control-point-1 'set-x xval1)
       (-- control-point-1 'set-y yval1)
       (-- control-point-2 'set-x xval2)
       (-- control-point-2 'set-y yval2)
       (-- destination-point 'set-x xval3)
       (-- destination-point 'set-y yval3)))
    (set-values*
     (lambda (x1-val y1-val z1-val x2-val y2-val z2-val x3-val y3-val z3-val)
       (-- control-point-1 'set-xyz* x1-val y1-val z1-val)
       (-- control-point-2 'set-xyz* x2-val y2-val z2-val)
       (-- destination-point-1 'set-xyz* x3-val y3-val z3-val)))))


;(defun 2d-path* (&rest data)
;  (if data
;      (! "2d-path*" `(path-list ,data))
;    (! "2d-path*")))

(defun moveto* (x-val y-val z-val)
  (! "move-to*" `(destination-point ,(! "3d-point" x-val y-val z-val))))

(defun lineto* (x-val y-val z-val &rest data)
  (if (memq 'corner data)
      (! "line-to*" `(destination-point ,(! "3d-point" x-val y-val z-val)) '(corner t))
  (! "line-to*" `(destination-point ,(! "3d-point" x-val y-val z-val)))))

(defun curveto* (x1-val y1-val z1-val x2-val y2-val z2-val x3-val y3-val z3-val &rest data)
  (let ((temp-curve (! "curve-to*")))
    (-- temp-curve 'set-values* x1-val y1-val z1-val x2-val y2-val z2-val x3-val y3-val z3-val) 
    (if (memq 'corner data)
	(-- temp-curve 'set 'corner t))))

(defun curve* (x-val y-val z-val &rest data)
  (let ((temp-curve (! "curve-to*")))
    (-- temp-curve 'set-values 0 0 0 0 0 0 x-val y-val z-val)
    (if (memq 'corner data)
	(-- temp-curve 'set 'corner t))
    temp-curve))

; I may not need this...
; (sos-class "2d-path*" "2d-path"
;   nil
;   nil
;   nil)

(sos-class "metaframe" "rectangle"
  '((location nil)
    (vp nil)
    (mp-e nil)
    (mp-ne nil)
    (mp-n nil)
    (mp-nw nil)
    (mp-w nil)
    (mp-sw nil)
    (mp-s nil)
    (mp-se nil)
    (line-1 nil); temp-vars
    (line-2 nil)
    (pt-1 nil)
    (pt-2 nil)
    (pt-3 nil)
    (temp-segment nil))
  '((init
     (lambda ()
      (if (not start-point)
	  (setq start-point (! "2d-point")))
       (if (not destination-point)
	   (setq destination-point (! "2d-point")))
       (if (not location)
	   (setq location (! "2d-point")))
       (if (not vp)
	   (setq vp (! "2d-point")))
       (if (not mp-e)
	   (setq mp-e (! "2d-point")))
       (if (not mp-ne)
	   (setq mp-ne (! "2d-point")))
       (if (not mp-n)
	   (setq mp-n (! "2d-point")))
       (if (not mp-nw)
	   (setq mp-nw (! "2d-point")))
       (if (not mp-w)
	   (setq mp-w (! "2d-point")))
       (if (not mp-sw)
	   (setq mp-sw (! "2d-point")))
       (if (not mp-s)
	   (setq mp-s (! "2d-point")))
       (if (not mp-se)
	   (setq mp-se (! "2d-point")))
       (if (not line-1)
	   (setq line-1 (! "line")))
       (if (not line-2)
	   (setq line-2 (! "line")))
       (if (not pt-1)
	   (setq pt-1 (! "2d-point")))
       (if (not pt-2)
	   (setq pt-2 (! "2d-point")))
       (if (not pt-3)
	   (setq pt-3 (! "2d-point")))
       (if (not temp-segment)
	   (setq temp-segment (! "line-segment")))
       (--- 'set-place+size 306 396 2000)))
 
    (process-points
     (lambda (a-point-list)
       (while a-point-list
	 (--- 'dynamic-message "metaframe: process points")
	 (--- 'xyz-to-xy (car a-point-list))
	 (setq a-point-list (cdr a-point-list)))))
    ;this method is simply a test version to prove the system logic.... xyz-to-xy is the last thing to be built.
    (xyz-to-xy 
     (lambda (3d-pnt)
       (let 
	   ((x* (-- 3d-pnt 'get-x*))
	   (y* (-- 3d-pnt 'get-y*))
	   (z* (-- 3d-pnt 'get-z*))
	   (x nil)
	   (y nil)
	   (mp nil)
	   (base-point pt-1)
	   (z-map-point pt-2)
	   (vp-x (-- vp 'get-x))
	   (vp-y (-- vp 'get-y)))
       (cond
	((zerop z*) 
	 (progn
	   (setq x x*)
	   (setq y y*)))
	((and (= x* vp-x) (= y* vp-y)) 
	 (progn
	   (setq x vp-x)
	   (setq y vp-y)))
	(t ; "if" block sets mp and z-map-point (vp is assumed)
	 (-- base-point 'set-xy x* y*)
	 (if (= y* vp-y)
	     (progn
	       (setq mp mp-s)
	       (-- z-map-point 'set-xy x* (+ y* z*)))
	   (if (> x* vp-x)
	       (progn
		 (if (> z* 0)
		     (setq mp mp-w)
		   (if (> y* vp-y) ; z is less than 0
		       (setq mp mp-nw)
		     (setq mp mp-sw)))
		 (-- z-map-point 'set-xy (+ x* (abs z*)) y*))
	     (progn
	       (if (> z* 0)
		   (setq mp mp-e)
		 (if (> y* vp-y) ; z is less than 0
		     (setq mp mp-ne)
		   (setq mp mp-se)))
	       (-- z-map-point 'set-xy (- x* (abs z*)) y*))))
	 (-- temp-segment 'set-data 
	     (-- vp 'get-x) (-- vp 'get-y)
	     (-- base-point 'get-x) (-- base-point 'get-y))
	 (-- temp-segment 'to-line line-1)
	 (-- temp-segment 'set-data 
	     (-- mp 'get-x) (-- mp 'get-y)
	     (-- z-map-point 'get-x) (-- z-map-point 'get-y))
	 (-- temp-segment 'to-line line-2)
	 (-- line-1 'xsect-line line-2 pt-3)
	 (setq x (-- pt-3 'get-x))
	 (setq y (-- pt-3 'get-y))))
	(-- 3d-pnt 'set-x x)
	(-- 3d-pnt 'set-y y))
     t))
    (set-place+size
     (lambda (x-val y-val a-size)
       (if x-val
	   (-- vp 'set-x x-val)
	 (setq x-val (-- vp 'get-x)))
       (if y-val
	   (-- vp 'set-y y-val)
	 (setq y-val (-- vp 'get-y)))
       (let ((coord nil ))       
	 (if a-size
	     (progn
	       (setq coord (abs (truncate (/ a-size 2)))) ; coord is half of a-size (centered on 0 0)
	       (--- 'set-data (- x-val coord) (- y-val coord) (+ x-val coord) (+ y-val coord))
	       (-- mp-e 'set-xy (+ x-val coord) y-val)
	       (-- mp-ne 'set-xy (+ x-val coord) (+ y-val coord))
	       (-- mp-n 'set-xy x-val (+ y-val coord))
	       (-- mp-nw 'set-xy (- x-val coord) (+ y-val coord))
	       (-- mp-w 'set-xy (- x-val coord) y-val)
	       (-- mp-sw 'set-xy (- x-val coord) (- y-val coord))
	       (-- mp-s 'set-xy x-val (- y-val coord))
	       (-- mp-se 'set-xy (+ x-val coord) (- y-val coord)))))))
))




(sos-class "perspective-frame" "frame"
  '((metaframe nil))
  '((init
     (lambda ()
       (set 'my-matrix (compact-sparse-matrix (make-identity-matrix 3)))
       (if (not bounds-rect)
	   (setq bounds-rect (-- (! "rectangle") 'set-data 0 0 612 792)))
       (if (not metaframe)
	   (setq metaframe (! "metaframe")))))
    (render*
     (lambda (&optional a-space)
       (let ((point-list nil))
	 (message "p-frame render*")
	 (setq point-list (--- 'collect-points))
;	 (print `("point list length: " ,(length point-list)))
	 (-- metaframe 'process-points point-list)
	 (--- 'render a-space)))) ; if nil, it just falls through
    (set-metaframe 
     (lambda (x-val y-val a-size)
       (-- metaframe 'set-place+size x-val y-val a-size)))
))





