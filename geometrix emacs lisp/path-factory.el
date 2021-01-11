; objects related to 2d-path generation


(sos-class "2d-path-factory" "vertex-element"
  '(
    ;(stroke? nil) ; redo on adv version to deal with graphics state as system global let
    (stroke-color 0)
    (stroke-line-width 1) 
    ;(fill? nil)
    (fill-color 1))
    ;(closed? nil))
  '((re-init
     (lambda ()
       (-- destination-point 're-init)
     ;  (setq stroke? nil)
       (setq stroke-color 0)
       (setq stroke-line-width 1)
     ;  (setq fill? nil)
       (setq fill-color 1)
     ;  (setq closed? nil)
       ))
    (path ; abstract method: produce path in descendents
     (lambda ()
       (message "*ERROR* path-factory: abstract method 'path called")))
    (get-text
     (lambda (&optional a-space))
       (message "*ERROR* path-factory: abstract method 'get-text called"))))

(sos-class "rectangle" "2d-path-factory"
  '((start-point nil))
  '((init
     (lambda ()
      (if (not start-point)
	  (setq start-point (! "2d-point")))
       (if (not destination-point)
	   (setq destination-point (! "2d-point")))
       (print `("rectangle init: start-point: " ,start-point))))
    (set-data
     (lambda (x1-val y1-val x2-val y2-val)
       (-- start-point 'set-x x1-val)
       (-- start-point 'set-y y1-val)
       (-- destination-point 'set-x x2-val)
       (-- destination-point 'set-y y2-val)
       self))
    (set-start-xy
     (lambda (x-val y-val)
       (-- start-point 'set-x x-val)
       (-- start-point 'set-y y-val)
       self))
    (set-destination-xy
     (lambda (x-val y-val)
       (-- destination-point 'set-x x-val)
       (-- destination-point 'set-y y-val)
       self))
    (path 
     (lambda ()
       (let ((x1 (-- start-point 'get-x))
	     (y1 (-- start-point 'get-y))
	     (x2 (-- destination-point 'get-x))
	     (y2 (-- destination-point 'get-y)))
	 (2d-path 
	  (moveto x1 y1)
	  (lineto x2 y1)
	  (lineto x2 y2)
	  (lineto x1 y2)
	  (lineto x1 y1)))))))



 