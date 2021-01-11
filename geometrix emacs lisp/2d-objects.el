; graphic objects for geometrix 1.0
; local-space
; 2d graphic objects consist of path data and a matrix that represents the
; location, rotation and scaling of the object.

; path data is represented in the local object space, and is used to 
; calculate paths for the containing frame or super-object space.

; abstract class defining object (space) manipulation methods and data
(sos-class "2d-object" "2d-space"
  `((local-space nil)
    (shadow-space nil)
;    (stroked? nil)
;    (filled? nil) 
    (stroked? nil) ; should default to nil - currently t for development
    (filled? nil) ; should default to nil - currently t for development
    (stroke-color nil)
    (stroke-width nil)
    (fill-color nil)
    (saved-stroke-color nil)
    (saved-stroke-width nil)
    (saved-fill-color nil)
    (grouped? t)
    (closed? t)
    (before-path nil)
    (after-path nil)
    (set-fill-color class ,(! "set-fill-color"))
    (reset-fill-color class ,(! "set-fill-color" '(pop? t)))
    (set-stroke-color class ,(! "set-stroke-color"))
    (reset-stroke-color class ,(! "set-stroke-color" '(pop? t)))
    (set-stroke-width class ,(! "set-stroke-width"))
    (reset-stroke-width class ,(! "set-stroke-width" '(pop? t)))
; test that these are appropriately configured path-elements
    (open-group class ,(! "group" '(start? t)))
    (close-group class ,(! "group" '(start? nil)))
    (fill-open-path class ,(! "paint" '(close nil) '(fill t)))
    (fill-closed-path class ,(! "paint" '(close t) '(fill t)))
    (stroke-open-path class ,(! "paint" '(close nil) '(stroke t)))
    (stroke-closed-path class ,(! "paint" '(close t) '(stroke t)))
    (fill-stroke-open-path class ,(! "paint" '(close nil) '(fill t) '(stroke t)))
    (fill-stroke-closed-path class ,(! "paint" '(close t) '(fill t) '(stroke t))))
  '((init
     (lambda ()
       (set 'my-matrix (compact-sparse-matrix (make-identity-matrix 3)))
       (set 'before-path (! "path"))
       (set 'after-path (! "path"))))
    (set-stroked?
     (lambda (bool)
       (if bool
	   (setq stroked? t)
	 (setq stroked? nil))))
    (set-filled?
     (lambda (bool)
       (if bool
	   (setq filled? t)
	 (setq filled? nil))))
    (set-closed?
     (lambda (bool)
       (if bool
	   (setq closed? t)
	 (setq closed? nil))))
    (set-grouped?
     (lambda (bool)
       (if bool
	   (setq grouped? t)
	 (setq grouped? nil))))
    (move
     (lambda (x-val y-val)
       (--- 'translate x-val y-val self)
       self))
    (move-to-point
     (lambda (a-point)
       (--- 'translate (-- a-point 'get-x)  (-- a-point 'get-y) self)
       self))
    (spin
     (lambda (an-angle)
       (if (not local-space)
	   (set 'local-space (! "2d-space")))
       (-- local-space 'rotate an-angle local-space)
       self))
    (size
     (lambda (a-scale-value); 1 is identity
       (if (not local-space)
	   (set 'local-space (! "2d-space")))
       (-- local-space 'scale a-scale-value a-scale-value local-space)
       self))
    (concat-space ; override ancestor
     (lambda (space-obj &optional dest-space)
       (if (not dest-space)
	   (setq dest-space (! "2d-space")))
       (-- dest-space 'set-to-space-matrix 
	   (if local-space
	       (sparse-m-times 
		(sparse-m-times (--- 'get-matrix-list) (-- space-obj 'get-matrix-list))
		(-- local-space 'get-matrix-list))
	     (sparse-m-times (--- 'get-matrix-list) (-- space-obj 'get-matrix-list))))
       dest-space))))
       
; local-space is used to do internal transformation of a particular space. 
; if the variable is nil, it does nothing, but if initialized with a 2d-space, 
; during render, it is concatenated with the shadow matrix (BEFORE the external 
; render matrix is applied (?))


(sos-class "frame" "2d-object"
  `((contents ())
    (bounds-rect nil)
    (clipping-path nil)
    (custom-clipping-path nil)
    (clipped? t) ; default to nil after working
    (background-fill-color .5)
    (outline-stroke-color .8)
    (outline-stroke-width 2.0)
; use with reset-fill-color, etc. (class-vars defined in "2d-space") to reset graphics state
    (set-background-fill-color class ,(! "set-fill-color"))
    (set-outline-stroke-color class ,(! "set-stroke-color"))
    (set-outline-stroke-width class ,(! "set-stroke-width"))

; test for appropriate configuration path-elements

    (open-clip class ,(! "clip" '(start? t)))
    (close-clip class ,(! "clip" '(start? nil)))
    (clip-to-path class ,(! "paint" '(clip t)))

)
  '((init after-ancestor
     (lambda ()
       (if (not bounds-rect)
	   (setq bounds-rect (! "rectangle")))
       (print `( "frame init: bounds-rect: " ,bounds-rect ,(-- bounds-rect 'get 'start-point)))
       (-- bounds-rect 'set-data 0 0 612 792)
       (if (not clipping-path)
	   (setq clipping-path (-- bounds-rect 'path)))))
    (build-before-after-paths
     (lambda ()
       (-- before-path 'clear-path)
       (-- after-path 'clear-path)
       ; set clipping path
       (if custom-clipping-path
	   (setq clipping-path custom-clipping-path)
	 (setq clipping-path (-- bounds-rect 'path)))
       ; do clipping
       (let ((g-state (--- 'get-class-var 'graphics-state)))
	 (if clipped?
	     (progn
	       (-- before-path 'add-element (--- 'get-class-var 'open-clip))
	       (-- before-path 'add-path clipping-path)
	       (-- before-path 'add-element (--- 'get-class-var 'clip-to-path))
	       (-- after-path 'add-element (--- 'get-class-var 'close-clip))))
	 (if grouped?
	     (progn
	       (-- before-path 'add-element (--- 'get-class-var 'open-group))
	       (-- after-path 'add-element-to-front (--- 'get-class-var 'close-group))))
	 (if filled?
	     (let ((fill-color-mod? nil))
	       (if (and background-fill-color (not (= (-- g-state 'get 'fill-color) background-fill-color))) 
		   (let ((fill-color-element (--- 'get-class-var 'set-background-fill-color)))
		     (setq fill-color-mod? t)
		     (-- fill-color-element 'set 'val background-fill-color)
		     (-- before-path 'add-element fill-color-element )))
	       (-- before-path 'add-path clipping-path)
	       (-- before-path 'add-element (--- 'get-class-var 'fill-closed-path))
	       (if fill-color-mod?
		   (-- before-path 'add-element (--- 'get-class-var 'reset-fill-color)))))
	 (if stroked?
	     (let ((stroke-color-mod? nil)(stroke-width-mod? nil));;; definition of stroke-path
	       (if (not (= (-- g-state 'get 'stroke-color) outline-stroke-color))
		   (let ((stroke-color-element (--- 'get-class-var 'set-outline-stroke-color)))
		     (setq stroke-color-mod? t)
		     (-- stroke-color-element 'set 'val outline-stroke-color)
		     (-- after-path 'add-element stroke-color-element )))
	       (-- test-out 'write-string (concat 
					   "frame ba-paths: outline-stroke-color " 
					   (number-to-string outline-stroke-color) 
					   " g-state stroke-color "
					   (number-to-string (-- g-state 'get 'stroke-color))
					   "\n"))
	       (if (not (= (-- g-state 'get 'stroke-width) outline-stroke-width)) 
		   (let ((stroke-width-element (--- 'get-class-var 'set-outline-stroke-width)))
		     (setq stroke-width-mod? t)
		     (-- stroke-width-element 'set 'val outline-stroke-width)
		     (-- after-path 'add-element stroke-width-element )))
	       (-- after-path 'add-path clipping-path)
	       (-- after-path 'add-element (--- 'get-class-var 'stroke-closed-path))

	       (if stroke-color-mod?
		   (-- after-path 'add-element (--- 'get-class-var 'reset-stroke-color)))
	       (if stroke-width-mod?
		   (-- after-path 'add-element (--- 'get-class-var 'reset-stroke-width)))))
	 ; if necessary, set g-state in before, then reset in after 
	 (if (and stroke-width (not (= (-- g-state 'get 'stroke-width) stroke-width)))
	     (let ((stroke-width-element (--- 'get-class-var 'set-stroke-width)))
	       (-- stroke-width-element 'set 'val stroke-width)
	       (-- before-path 'add-element stroke-width-element )
	       (-- after-path 'add-element-to-front (--- 'get-class-var 'reset-stroke-width))))
	 (if (and stroke-color (not (= (-- g-state 'get 'stroke-color) stroke-color)))
	     (let ((stroke-color-element (--- 'get-class-var 'set-stroke-color)))
	       (-- stroke-color-element 'set 'val stroke-color)
	       (-- before-path 'add-element stroke-color-element )
	       (-- after-path 'add-element-to-front (--- 'get-class-var 'reset-stroke-color))))
	 (if (and fill-color (not (= (-- g-state 'get 'fill-color) fill-color))) 
	     (let ((fill-color-element (--- 'get-class-var 'set-fill-color)))
	       (-- fill-color-element 'set 'val fill-color)
	       (-- before-path 'add-element fill-color-element )
	       (-- after-path 'add-element-to-front (--- 'get-class-var 'reset-fill-color)))))



;       (if grouped?
;	     (-- before-path 'add-element (--- 'get-class-var 'open-group)))
;       (if clipped?
;	   (progn
;	     (-- before-path 'add-element (--- 'get-class-var 'open-clip))
;	     (-- before-path 'add-path clipping-path)
;	     (-- before-path 'add-element (--- 'get-class-var 'clip-to-path))))

       ; build after-path
;       (if clipped?
;	   (-- after-path 'add-element (--- 'get-class-var 'close-clip)))
;       (if stroked?
;	   (progn
;	     (-- after-path 'add-path clipping-path)
;	     (-- after-path 'add-element (--- 'get-class-var 'stroke-closed-path))))
;       (if grouped?
;	   (-- after-path 'add-element (--- 'get-class-var 'close-group)))

))
    (set-bounds-rect ; bounds values are valid in local space
     (lambda (x1 y1 x2 y2)
       (-- bounds-rect 'set-data x1 y1 x2 y2)
       (if (not custom-clipping-path)
	   (setq clipping-path (-- bounds-rect 'path)))))
    (set-custom-clipping-path ; bounds values are valid in local space
     (lambda (a-path)
       (setq custom-clipping-path a-path)))
    (set-clipped?
     (lambda (a-bool)
       (if a-bool
	   (setq clipped? t)
	 (setq clipped? nil))))
    (add-element
     (lambda (an-element)
       (setq contents (append contents (list an-element)))
       self))
    (add-elements
     (lambda (an-element &rest element-list)
       (setq contents (append (cons an-element element-list) contents)) ; if element-list is nil, this should still work
       self))
    (render
     (lambda (&optional a-space)
       (--- 'build-before-after-paths)
       (if (not shadow-space)
	   (setq shadow-space (! "2d-space"))
	 (-- shadow-space 're-init))
       (let ((my-contents contents))
	 (if a-space 
	     (--- 'concat-space a-space shadow-space)
	   (--- 'concat-space shadow-space shadow-space))
	 (-- before-path 'render shadow-space)
	 (while my-contents
	   (-- (car my-contents) 'render shadow-space)
	   (setq my-contents (cdr my-contents))))
	 (-- after-path 'render shadow-space)

       self))
    (clear-contents
     (lambda ()
       (setq contents '())
       self))
    (collect-points ; this is initially for xyz-to-xy metaframe stuff, but could be used for other point processing schemes
     (lambda ()
       (let ((temp-contents contents)(point-list nil))
	 (while temp-contents
	   (--- 'dynamic-message "collect-points")
	   (setq point-list (-- (car temp-contents) 'contribute-points point-list))
	   (setq temp-contents (cdr temp-contents)))
	 point-list)))

))
	   

(sos-class "2d-path" "2d-object"
  `((my-path nil))
  '((init after-ancestor
     (lambda ()    
       (set 'my-path (! "path"))))
    (build-before-after-paths ; invalid unless called at beginning of render
     (lambda ()
;       (message "2d-path: build-before-after-paths")
       (-- before-path 'clear-path)
       (-- after-path 'clear-path)
       (let ((g-state (--- 'get-class-var 'graphics-state)))
	 ; if necessary, set g-state in before, then reset in after 
	 (if (and stroke-width (not (= (-- g-state 'get 'stroke-width) stroke-width)))
	     (let ((stroke-width-element (--- 'get-class-var 'set-stroke-width)))
	       (-- stroke-width-element 'set 'val stroke-width)
	       (-- before-path 'add-element stroke-width-element)
	       (-- after-path 'add-element (--- 'get-class-var 'reset-stroke-width))))
	 (if (and stroke-color (not (= (-- g-state 'get 'stroke-color) stroke-color))) 
	     (let ((stroke-color-element (--- 'get-class-var 'set-stroke-color)))
	       (-- stroke-color-element 'set 'val stroke-color)
	       (-- before-path 'add-element stroke-color-element )
	       (-- after-path 'add-element (--- 'get-class-var 'reset-stroke-color))))
	 (if (and fill-color (not (= (-- g-state 'get 'fill-color) fill-color))) 
	     (let ((fill-color-element (--- 'get-class-var 'set-fill-color)))
	       (-- fill-color-element 'set 'val fill-color)
	       (-- before-path 'add-element fill-color-element )
	       (-- after-path 'add-element (--- 'get-class-var 'reset-fill-color))))
 

       (if grouped?
	   (-- before-path 'add-element (--- 'get-class-var 'open-group)))
       (if grouped?
	   (-- after-path 'add-element (--- 'get-class-var 'close-group)))
;debug code
       (let ((message-string "\n"))
	 (if grouped?
	     (setq message-string (concat "grouped " message-string)))
	 (if stroked?
	     (setq message-string (concat "stroked " message-string)))
	 (if stroke-width
	     (setq message-string (concat " width: " (number-to-string stroke-width) " " message-string)))
	 (if stroke-color
	     (setq message-string (concat " color: " (number-to-string stroke-color) " " message-string)))
	 (if filled?
	     (setq message-string (concat "filled " message-string)))
	 (if fill-color
	     (setq message-string (concat " color: " (number-to-string fill-color) " " message-string)))
	 (if closed?
	     (setq message-string (concat "closed " message-string)))
	 (setq message-string (concat "2d-path paint values: " message-string))
;	 (message message-string)
)

       (if stroked?
	   (if filled?
	       (if closed?
		   (-- after-path 'add-element-to-front (--- 'get-class-var 'fill-stroke-closed-path))
		 (-- after-path 'add-element-to-front (--- 'get-class-var 'fill-stroke-open-path)))
	     (if closed?
		 (-- after-path 'add-element-to-front (--- 'get-class-var 'stroke-closed-path))
	       (-- after-path 'add-element-to-front (--- 'get-class-var 'stroke-open-path))))
	 (if filled?
	       (if closed?
		   (-- after-path 'add-element-to-front (--- 'get-class-var 'fill-closed-path))
		 (-- after-path 'add-element-to-front (--- 'get-class-var 'fill-open-path)))
;	  (message  "\nERROR:\npath: build-before-after-paths -- invalid paint setting")
	   ))
;       (print `( "2d-path build-before-after-paths after-path list: " ,(-- after-path 'get-path-list)))
)))
    (get-path-list
     (lambda ()
       (-- my-path 'get-path-list)))
    (smooth
     (lambda ()
       (-- my-path 'smooth)
       self))
    (output-text-list
     (lambda ()
       (-- my-path 'output-text-list)))
    (render 
     (lambda (a-space)
       (--- 'build-before-after-paths)
       (-- before-path 'render)
       (if (not shadow-space)
	   (setq shadow-space (! "2d-space"))
	 (-- shadow-space 're-init))
       (--- 'concat-space a-space shadow-space)
       (-- my-path 'render shadow-space)
       (-- after-path 'render)
       (--- 'dynamic-message "2d-path render")
       self))
    (clear-path 
     (lambda ()
       (-- my-path 'clear-path)
       self))
    (add-path-list
     (lambda (a-data-list)
       (-- my-path 'add-path-list a-data-list)
       self))
    (add-element
     (lambda (an-element)
       (-- my-path 'add-element an-element)
       self))
    (add-element-to-front
     (lambda (an-element)
       (-- my-path 'add-element-to-front an-element)))
    (contribute-points
     (lambda (a-list)
       (setq a-list (-- my-path 'contribute-points a-list))
       a-list))

))

(defun 2d-path (&rest elements)
  (if elements
      (let ((new-2d-path (! "2d-path")))
	(-- new-2d-path 'add-path-list elements)
	new-2d-path)
    (! "2d-path")))


