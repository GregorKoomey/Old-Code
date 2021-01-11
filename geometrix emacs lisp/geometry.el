; geometry objects

(sos-class "2d-point" "object"
  '((x 0)(y 0)(temp-point class))
  '(
    (init
     (lambda ()
       (set 'x (float x))
       (set 'y (float y))))
    (re-init
     (lambda ()
       (set 'x 0.0)
       (set 'y 0.0)))
    (get-text
     (lambda (&optional a-space)
       (let ((xval x) (yval y) temp-point)
	 (if a-space
	     (progn
	       (setq temp-point (--- 'get-class-var 'temp-point))
	       (-- a-space 'transform self temp-point)
	       (setq xval (-- temp-point 'get-x))
	       (setq yval (-- temp-point 'get-y))))
	 (concat 
	  (number-to-string (--- 'clip-float xval))
	  " "
	  (number-to-string (--- 'clip-float yval))
	  " "))))
    (output-text
     (lambda ()
      (concat 
       (number-to-string x)
       " "
       (number-to-string y)
       " ")))
    (get-x
     (lambda()
       x))
    (get-y
     (lambda()
       y))
    (set-xy 
     (lambda (x-val y-val)
	(set 'x (float x-val))
	(set 'y (float y-val))))
    (set-x 
     (lambda (val)
	(set 'x (float val))))   
    (set-y 
     (lambda (val)
	(set 'y (float val))))
    (clip-float 
     (lambda (float-value)
       (let ((temp-value (* 1000 float-value)))
	 (setq temp-value (float (truncate temp-value)))
	 (/ temp-value 1000))))))

(let ((a-point (! "2d-point")))
  (sos-set-class-var "2d-point" 'temp-point a-point))

; 3d-point is intended to be used with metaframe perspective system
;  intention is to establish using 3d coords, then transform at (or before) 
;  render, using xyz-to-xy method that stores x' y' back into x and y of point.
;   thus, render path functionality can work as it has with fatter data...

(sos-class "3d-point" "2d-point"
  '((x* 0) (y* 0) (z* 0))
  '((init
     (lambda ()
       (set 'x (float x))
       (set 'y (float y))
       (set 'x* (float x*))
       (set 'y* (float y*))
       (set 'z* (float z*))))
    (re-init
     (lambda ()
       (set 'x 0.0)
       (set 'y 0.0)
       (set 'x* 0.0)
       (set 'y* 0.0)
       (set 'z* 0.0)))
    (set-xyz*
     (lambda (x-val y-val z-val)
       (set 'x* (float x-val))
       (set 'y* (float y-val))
       (set 'z* (float z-val))
       (set 'x (float x-val))
       (set 'y (float y-val))))
    (set-xy ; override ancestor
     (lambda (x-val y-val)
	(set 'x (float x-val))
	(set 'y (float y-val))
	(set 'x* (float x-val))
	(set 'y* (float y-val))
	(set 'z* 0.0)))
     (set-x* 
      (lambda (val)
	(set 'x* (float val))))
     (set-y* 
      (lambda (val)
	(set 'y* (float val))))
     (set-z* 
      (lambda (val)
	(set 'z* (float val))))
     (get-x* 
      (lambda ()
	x*))
     (get-y* 
      (lambda ()
	y*))
     (get-z* 
      (lambda ()
	z*))
    (output-text
     (lambda ()
      (concat 
       "xy: "
       (number-to-string x)
       " "
       (number-to-string y)
       "  xyz* :"
       (number-to-string x*)
       " "
       (number-to-string y*)
       " "
       (number-to-string z*)
       " ")))
))


(sos-set-class-var "3d-point" 'parameter-order '(x* y* z* x y))


(sos-class "angle" "object"
  '((ang 0))
  '((get-text
     (lambda ()
       (concat (number-to-string ang) " ")))
    (set-ang ; set angle in degrees
     (lambda (value)
       (progn
	 (set 'ang (float value))
	 (--- 'constrain))))
    (re-init
     (lambda ()
       (setq ang 0)))
    (get-ang ; return angle value in degrees 
     (lambda ()
       ang))
    (add-ang
      (lambda (value)
	(set 'ang (+ ang value))
	(--- 'constrain)))
    (sub-ang
      (lambda (value)
	(set 'ang (- ang value))
	(--- 'constrain)))
    (sub-ang-from
      (lambda (value)
	(set 'ang (- value ang))
	(--- 'constrain)))    
    (get-radians ; return angle value in radians
     (lambda()
       (/ ang 57.295779513082)))
    (get-inverse
     (lambda()
       (let ((inverse-val (+ ang 180)))
	 (while (< inverse-val 0)
	   (setq inverse-val (+ inverse-val 360)))
	 (while (> inverse-val 360)
	   (setq inverse-val (- inverse-val 360)))
	 inverse-val)))
    (constrain ; maintain limit of 0-360 degrees
     (lambda()
       (let ((value ang))
	 (while (< value 0)
	   (setq value (+ value 360)))
	 (while (> value 360)
	   (setq value (- value 360)))
	 (set 'ang value))))
    (set-ang-from-radian ; set angle from radian value
     (lambda(rads)
       (set 'ang (* rads 57.295779513082))
       (--- 'constrain)))
    (perpendicular ; return perpendicular angle object
     (lambda (&optional newang)
       (progn
	 (if (not newang)
	     (setq newang (! "angle")))
	 (-- newang 'set-ang (+ ang 90))
	 (-- newang 'constrain)
	 newang)))
    (sin ; return sin value of angle
     (lambda ()
       (sin (--- 'get-radians))))
    (cos ; return cos value of angle
     (lambda ()
       (cos (--- 'get-radians))))
    (tan ; return tan value of angle
     (lambda ()
       (let ((curr-ang-value ang))
	 (if (not (or (= curr-ang-value 90.0) (= curr-ang-value 270.0)))
	     (progn
	       (if (and (> curr-ang-value 90) (<= curr-ang-value 270))
		   (set 'curr-ang-value (- curr-ang-value 180))
		 (if (and (> curr-ang-value 270)(<= curr-ang-value 360))
		     (set 'curr-ang-value (- curr-ang-value 360))))
	       (tan (degrees-to-radians curr-ang-value)))
	   nil))))
    (asin-set ; set angle from asin of value: range [-90  90] 
     (lambda(val)
       (--- 'set-ang-from-radian (asin val))))
    (acos-set  ; set angle from acos of value: range [0 180]
     (lambda(val)
       (--- 'set-ang-from-radian (acos val))))
    (atan-set ; set angle from atan of value: range [-90 90]
     (lambda(val)
       (--- 'set-ang-from-radian (atan val))))
    (get-slope 
     (lambda ()
       (--- 'tan)))
;override equiv? to check for (valid) inverse angle
    (equiv?
     (lambda (other)
       (if (sos-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	     (maphash
	      (lambda (key value)
		(if (sos-instance? value)
		    (setq obj-check (-- value 'equiv? (-- other 'get key))))
		(if 
		    (not 
		     (or 
		      (if (equal key 'ang)
			  (let ((temp-angle-value (--- 'get-inverse)))
			    (equal (-- other 'get key) temp-angle-value)))
		      (equal 
		       (-- other 'get key) 
		       value) 
		      obj-check))
		    (setq y-or-n nil)))
	      self)
	     y-or-n)
	 nil)))
    (pt-to-line 
     (lambda (pt &optional aline) ; pt is a point object ; b = y - mx
       (let ((new-line nil)
	     (my-angle-value ang)
	     (line-angle nil)
	     (x (-- pt 'get-x))
	     (y (-- pt 'get-y)))
	 (setq new-line 
	       (if aline
		   aline
		 (! "line")))
	 (setq line-angle (-- new-line 'get-angle))
	 (-- line-angle 'set-ang my-angle-value)
	 (setq my-angle-value (-- line-angle 'get-ang)); constrain requires reflection
	 (-- new-line 'calc-slope)
	 (if (or (= my-angle-value 90)(= my-angle-value 270))
	     (progn
	       (-- new-line 'set-b nil)
	       (-- new-line 'set-x x))

	   (-- new-line 'set-b (- y (* (-- new-line 'get-m) x))))
;	 (print `("angle: pt-to-line Pt:" ,(-- pt 'get-text) " Line:" ,(-- new-line 'output-text)))
	 new-line
       )))))



; Abstract line object: based on y = mx + b
; angle is angle object
; m is derived from angle object (tan angle-value), corrected for domain,
; b and x are either numbers or nil (one or the other is always nil)
;   b refers to the point of intersection of the line with the y-axis...
;   irregular lines:
;    when angle is either 90 or 270, m and b are undefined and so the line is fully
;    defined by the optional x value, which is the intersection with the x axis
;    irregular lines are set externally ie.: '(angle 90/270) '(m nil) '(b nil) '(x 5)

(sos-class "line" "object" 
  '((angle nil)(m nil)(b 0)(x nil))
  '((init
     (lambda()
       (set 'angle (! "angle"))))
    (re-init
     (lambda ()
       (-- angle 're-init)
       (setq m 0)
       (setq b 0)
       (setq x nil)))
    (smooth nil)
    (irregular? 
     (lambda ()
       (eq b nil)))
    (set-angle
     (lambda (value-angle); angle object
       (set 'angle value-angle)
       (--- 'calc-slope)))
    (get-angle
     (lambda ()
       angle))
    (set-b 
     (lambda (value)
       (set 'b value)))
    (get-b 
     (lambda ()
       b))
    (get-m
     (lambda ()
       m))
    (set-x 
     (lambda (value)
       (set 'x value)))
    (get-x 
     (lambda ()
       x))
    (set-ang
     (lambda (value)
       (-- angle 'set-ang value)))
    (get-ang
     (lambda ()
       (-- angle 'get-ang)))
    (calc-y
     (lambda (x-val)
       (--- 'calc-slope)
       (if m
	   (+ (* m x-val) b)
	 nil)))
    (calc-slope
     (lambda ()
       (set 'm (-- angle 'get-slope))
       m))
;    (set-slope
;     (lambda (value) ; angle number value
;       (-- m 'set-angle value)))
    (xsect-line ; returns new point or nil: ADV-allow prefab point to hold value
     (lambda (aline &optional apoint)
       (if aline
	   (let 
	       ((new-point apoint) new-x)
;	     (message (-- aline 'output-text))
	     (--- 'calc-slope)
	     (-- aline 'calc-slope)
	     (if (not (-- angle 'equiv? (-- aline 'get-angle)))
		 (progn
		   (if (not new-point)
		       (setq new-point (! "2d-point")))
		   (if (-- self 'irregular?)
		       (progn 
			 (setq new-x x)
			 (-- new-point 'set-x new-x)
			 (-- new-point 'set-y (-- aline 'calc-y new-x)))
		     (if (-- aline 'irregular?)
			 (progn
			   (setq new-x (-- aline 'get-x))
			   (-- new-point 'set-x new-x)
			   (-- new-point 'set-y (--- 'calc-y new-x)))
		       (let ((my-slope m)
			     (my-b b)
			     (my-x x)
			     (oth-slope (-- aline 'get-m))
			     (oth-b (-- aline 'get-b))
			     (oth-x (-- aline 'get-x)))
			 (setq new-x
			       (/
				(- (-- aline 'get-b) my-b)
				(- my-slope oth-slope)))
			 (-- new-point 'set-x new-x)
			 (-- new-point 'set-y 
			     (+ (* my-slope new-x) b)))))))
	     new-point)
	 nil))) ; note: returns nil when not initialized through logic
    (output-text
     (lambda ()
       (concat 
	"Angle: "
	(-- angle 'get-text) 
	" m:" 
	(if m
	    (number-to-string m) 
	  "nil")
	" b:"
	(if b
	    (number-to-string b) 
	  "nil")
	" x:"
	(if x
	    (number-to-string x) 
	  "nil")
	" ")))))

(defun line (angle-value y-axis-intercept &optional x-value)
  (let ((new-line (! "line" `(b  ,y-axis-intercept) `(x ,x-value)))
	(new-angle (! "angle")))
    (-- new-angle 'set-ang angle-value)
    (-- new-line 'set 'angle new-angle)
    new-line))

(sos-class "line-segment" "object"
  `((start nil)
    (end nil)
    (line-mode nil)
    (utility-angle class))
  '(
    (init
     (lambda ()
       (if (not start)
	   (setq start (! "2d-point")))
       (if (not end)
	   (setq end (! "2d-point")))))
    (re-init
     (lambda ()
       (-- start 're-init)
       (-- end 're-init)))

    (output-text
     (lambda()
      (concat
       "start:"
       (-- start 'get-text)
       " end:"
       (-- end 'get-text))))

     (set-x1
      (lambda (value)
       (-- start 'set-x value)))
    (set-y1
     (lambda (value)
       (-- start 'set-y value)))
    (set-x2
     (lambda (value)
       (-- end 'set-x value)))
    (set-y2
     (lambda (value)
       (-- end 'set-y value)))
    (set-data
     (lambda (x1 y1 x2 y2)
       (-- start 'set-x x1)
       (-- start 'set-y y1)
       (-- end 'set-x x2)
       (-- end 'set-y y2)))
    (length 
     (lambda ()
       (let (
	     (x1 (-- start 'get 'x))
	     (y1 (-- start 'get 'y))
	     (x2 (-- end 'get 'x))
	     (y2 (-- end 'get 'y))
	     (dx nil)
	     (dy nil))
	 (setq dx (- x2 x1))
	 (setq dy (- y2 y1))
	 (sqrt (+ (* dx dx) (* dy dy))))))
    (mid-point 
     (lambda (&optional apoint)
       (let (
	     (x1 (-- start 'get 'x))
	     (y1 (-- start 'get 'y))
	     (x2 (-- end 'get 'x))
	     (y2 (-- end 'get 'y))
	     (new-x nil)
	     (new-y nil))
	 (setq new-x (/ (+ x1 x2) 2))
	 (setq new-y (/ (+ y1 y2) 2))
	 (if apoint
	     (progn
	       (-- apoint 'set-x new-x)
	       (-- apoint 'set-y new-y))
	 (2d-point  new-x new-y)))))
    (to-line 
     (lambda (&optional aline)
       (let ((dx (- (-- end 'get-x) (-- start 'get-x)))
	     (dy (- (-- end 'get-y) (-- start 'get-y))))
	 (set 'line-mode nil)
;	 (print `(,dx ,dy))
	 (if (= dx 0)
	     (if (= dy 0)
		 (progn
		   (set 'line-mode 'line-error))
	       (if (< dy 0)
		   (set 'line-mode '0:-inf)
		 (set 'line-mode '0:inf)))
	   (progn 
	     (if (= dy 0)
	       (if (< dx 0)
		   (set 'line-mode '-inf:0)
		 (set 'line-mode 'inf:0))
	       (if (< dx 0)
		   (if (< dy 0)
		       (set 'line-mode '-:-)
		     (set 'line-mode '-:+))
		 (if (< dy 0)
		     (set 'line-mode '+:-)
		   (set 'line-mode '+:+))))))
;	 (print line-mode)
	   (let ((line-inst aline))
	     (if (not line-inst)
		 (setq line-inst (! "line")))
;	     (print line-inst)
	     (setq line-inst (--- line-mode dx dy line-inst))
	     (if line-inst
		 (-- line-inst 'calc-slope))
	     line-inst))))
; line calculation methods (do not use explicitly... used only by to-line method)
; return new-line or nil

    (-:-
     (lambda (dx dy new-line)
       (let ((new-angle 
	       (-- new-line 'get-angle) 
	       ))
	 (-- new-angle 'set-ang (--- 'get-angle-in-degrees dx dy))
	 (-- new-angle 'set-ang (- 90.0 (-- new-angle 'get-ang)))
	 (-- new-angle 'add-ang 180)
	 (-- new-line 'calc-slope)
	 (-- new-angle 'pt-to-line start new-line)
	 new-line)))

    (-:+
     (lambda (dx dy new-line)
       (let ((new-angle 
	       (-- new-line 'get-angle) 
	       ))
	 (-- new-angle 'set-ang (--- 'get-angle-in-degrees dx dy))
	 (-- new-angle 'add-ang 90)
	 (-- new-line 'calc-slope)
	 (-- new-angle 'pt-to-line start new-line)
	 new-line)))

    (+:-
     (lambda (dx dy new-line)
       (let ((new-angle 
	       (-- new-line 'get-angle) 
	       ))
	 (-- new-angle 'set-ang (--- 'get-angle-in-degrees dx dy))
	 (-- new-angle 'add-ang 270)
	 (-- new-line 'calc-slope)
	 (-- new-angle 'pt-to-line start new-line)
	 new-line)))

    (+:+
     (lambda (dx dy new-line)
       (let ((new-angle 
	       (-- new-line 'get-angle) 
	       ))
	 (-- new-angle 'set-ang (--- 'get-angle-in-degrees dx dy))
	 (-- new-angle 'set-ang (- 90.0 (-- new-angle 'get-ang)))
	 (-- new-line 'calc-slope)
	 (-- new-angle 'pt-to-line start new-line)
	 new-line)))
     (0:inf
      (lambda (dx dy new-line)
	(-- (-- new-line 'get-angle) 'set-ang 90.0) 
	(-- new-line 'set 'b nil)
	(-- new-line 'set 'x (-- start 'get-x))
	new-line))
     (0:-inf
      (lambda (dx dy new-line)
	(-- (-- new-line 'get-angle)  'set-ang 270.0) 
	(-- new-line 'set 'b nil)
	(-- new-line 'set 'x (-- start 'get-x))
	new-line))
     (inf:0
      (lambda (dx dy new-line)
	(-- (-- new-line 'get-angle)  'set-ang 0.0) 
	(-- new-line 'set 'b (-- start 'get-y))
	(-- new-line 'set 'x nil)
	new-line))
     (-inf:0
      (lambda (dx dy new-line)
	(--(-- new-line 'get-angle)  'set-ang 180.0) 
	(-- new-line 'set 'b (-- start 'get-y))
	(-- new-line 'set 'x nil)
	new-line))
     (line-error 
      (lambda (dx dy new-line)
	(message "line-seg get-line error: points are same")
	nil))
     (get-angle-in-degrees
      (lambda (dx dy)
	(radians-to-degrees (--- 'get-angle-in-radians dx dy))))
    (get-angle-in-radians
     (lambda (dx dy)
       (atan (/ (abs dx)(abs dy)))))
    (get-ang 
     (lambda ()
       (let ((dx (- (-- end 'get-x) (-- start 'get-x)))
	     (dy (- (-- end 'get-y) (-- start 'get-y))))
	 (-- self 'get-angle-in-degrees dx dy))))
    (get-angle ; allocates new angle object
     (lambda (&rest my-angle)
       (progn
	 (if (not my-angle) (setq my-angle (! "angle")))
	 (-- my-angle 'set-ang (--- 'get-ang))
	 my-angle)))))

(sos-set-class-var "line-segment" 'utility-angle (! "angle"))




(defun line-segment (x1 y1 x2 y2)
  (let ((new-line-seg (! "line-segment"))
	(pt1 nil)
	(pt2 nil))
    (set 'pt1 (-- new-line-seg 'get 'start))
    (set 'pt2 (-- new-line-seg 'get 'end))
    (-- pt1 'set-x x1)
    (-- pt1 'set-y y1)
    (-- pt2 'set-x x2)
    (-- pt2 'set-y y2)
    new-line-seg))
    


