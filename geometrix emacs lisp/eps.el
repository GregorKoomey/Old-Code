; objects that model the creation and execution of illustrator 3 eps files
;   relies on sos, standard

; EPS System Object Functions:
;  System is defined around sos objects that are created (and sometimes manipulated) 
;  through global function calls

;Stage One
;   value - parent class to primitive system parts that require textual representation 
;     in output - text-value method 

; *color not fully working*
;   color <- value 
;     fn: (grayscale-color grayvalue)

;   2d-point <- value

;   path (contains path-elements)
;     fn: (path element1 element2 subpath...etc.)

;   path-elements:
;     move-to
;       fn: (moveto point1)
;     line-to
;       fn: (lineto point1)
;     curve-to
;       fn: (curveto point1 point2 point3)
;     paint
;       fn: (paint 'close 'fill 'stroke)


;Stage Two
;  
; Classes
; 
;   angle

;*accomplished-to-here

;   line
;
;   line-seg

;   curve-frame
;
;   point-list

; Mods
;   2d-Point
;     pt-to-line(pt) => <line>
;     slope-to-line(angle) => <line>
;
;   line/line-seg
;     xsect must respond to either

;   object
;     ancestry? method
;     clone method


; Next Phase: spigot




; path and path elements


(sos-class "curve-assembler" "object"
  '((previous-2 nil)
    (previous nil)
    (current nil)
    (next nil)
    (condition nil)
    ; resource data
    (line-a nil)
    (line-b nil)
    (line-c nil)
    (line-d nil)
    (line-e nil)
    (line-f nil)
    (seg-a nil)
    (seg-b nil)
    (seg-c nil)
    (seg-d nil)
    (seg-e nil)
    (seg-f nil)
    (seg-g nil)
    (seg-h nil)
    (seg-i nil)
    (pt-a nil)
    (pt-b nil)
    (pt-c nil)
    (ang-a nil)
    (ang-b nil)
    (ang-c nil)
    (mode-a nil)
    (mode-b nil)
    (ctrl-pt-1 nil)
    (ctrl-pt-2 nil)
    (condition-hash nil))
  '((init
     (lambda ()
       (setq line-a (! "line"))
       (setq line-b (! "line"))
       (setq line-c (! "line"))
       (setq line-d (! "line"))
       (setq line-e (! "line"))
       (setq line-f (! "line"))
       (setq seg-a (! "line-segment"))
       (setq seg-b (! "line-segment"))
       (setq seg-c (! "line-segment"))
       (setq seg-d (! "line-segment"))
       (setq seg-e (! "line-segment"))
       (setq seg-f (! "line-segment"))
       (setq seg-g (! "line-segment"))
       (setq seg-h (! "line-segment"))
       (setq seg-i (! "line-segment"))
       (setq pt-a (! "2d-point"))
       (setq pt-b (! "2d-point"))
       (setq pt-c (! "2d-point"))
       (setq ang-a (! "angle"))
       (setq ang-b (! "angle"))
       (setq ang-c (! "angle"))
       (setq condition-hash (make-hash-table))
       (let ((straight-hash (make-hash-table))
	     (start-hash (make-hash-table))
	     (clock-hash (make-hash-table))
	     (counter-clock-hash (make-hash-table)))
	 (puthash 'straight straight-hash condition-hash)
	 (puthash 'straight 'straight-method straight-hash)
	 (puthash 'end 'straight-method straight-hash)
;	 (puthash 'clock 'straight-rounded-method straight-hash)
;	 (puthash 'counter-clock 'straight-rounded-method straight-hash)
	 (puthash 'clock 'straight-method straight-hash)
	 (puthash 'counter-clock 'straight-method straight-hash)

	 (puthash 'start start-hash condition-hash)
	 (puthash 'straight 'straight-method start-hash)
	 (puthash 'counter-clock 'start-rounded-method start-hash)
	 (puthash 'clock 'start-rounded-method start-hash)

	 (puthash 'clock clock-hash condition-hash)
	 (puthash 'clock 'rounded-method clock-hash)
	 (puthash 'counter-clock 'skewed-method clock-hash)
;	 (puthash 'straight 'rounded-straight-method clock-hash)
	 (puthash 'straight 'straight-method clock-hash)
	 (puthash 'end 'rounded-end-method clock-hash)

	 (puthash 'counter-clock counter-clock-hash condition-hash)
	 (puthash 'counter-clock 'rounded-method counter-clock-hash)
	 (puthash 'clock 'skewed-method counter-clock-hash)
;	 (puthash 'straight 'rounded-straight-method counter-clock-hash)
	 (puthash 'straight 'straight-method counter-clock-hash)
	 (puthash 'end 'rounded-end-method counter-clock-hash))))
    (fire 
     (lambda (curve-frame)
;       (message "Start fire: ")
       (setq mode-a nil)
       (setq mode-b nil)

       (setq previous-2 (car curve-frame))
       (setq previous (car (cdr curve-frame)))
       (setq current (car (cdr (cdr curve-frame))))
       (setq next (car (cdr (cdr (cdr curve-frame)))))




;       (print `(,(if previous-2 (-- previous-2 'get-text) nil)
;		,(if previous (-- previous 'get-text) nil)
;		,(if current (-- current 'get-text) nil)
;		,(if next (-- next 'get-text) nil)))

 ;      (print `(,previous-2 "\n" ,previous  "\n" ,current  "\n" ,next))


       (if previous-2
	   (-- seg-a 'set-data 
	       ( -- previous-2 'get-x)
	       ( -- previous-2 'get-y)
	       ( -- previous 'get-x)
	       ( -- previous 'get-y))
	 (setq mode-a 'start))
       (-- seg-b 'set-data 
	   ( -- previous 'get-x)
	   ( -- previous 'get-y)
	   ( -- current 'get-x)
	   ( -- current 'get-y))
       (if next
	   (-- seg-c 'set-data 
	       ( -- current 'get-x)
	       ( -- current 'get-y)
	       ( -- next 'get-x)
	       ( -- next 'get-y))
	 (setq mode-b 'end))


;;;a problem here... the values printed are correct, yet the mode-a mode-b condition values are wrong
       (print `(,(if seg-a (-- seg-a 'output-text) nil)
		,(if seg-b (-- seg-b 'output-text) nil)
		,(if seg-c (-- seg-c 'output-text) nil)))
	      

       (if (not mode-a)
	   (progn
	     (-- seg-a 'to-line line-a)
	     (-- seg-b 'to-line line-b)
	     (setq mode-a (--- 'winding? (-- line-a 'get-ang) (-- line-b 'get-ang)))))
       (if (not mode-b)
	   (progn
	     (-- seg-b 'to-line line-b)
	     (-- seg-c 'to-line line-c)
	     (setq mode-b (--- 'winding? (-- line-b 'get-ang) (-- line-c 'get-ang)))))


       (setq condition (gethash mode-b (gethash mode-a condition-hash)))

       (print `(,mode-a ,mode-b ,condition))
;;;;; is winding wrong?





       (setq ctrl-pt-1 (-- current 'get 'control-point-1))
       (setq ctrl-pt-2 (-- current 'get 'control-point-2))

      (let ((pt1 (-- seg-a 'get 'start))
	     (pt2 (-- seg-b 'get 'start))
	     (pt3 (-- seg-b 'get 'end))
	     (pt4 (-- seg-c 'get 'end)))
       (--- condition))

       (--- 'reset)

       ))
    (winding? ; appears to be broken
     (lambda (ang1 ang2)
       (message (concat "winding angles: " (number-to-string ang1) " " (number-to-string ang2) "\n"))
       (let ((value nil) (angx nil))
	 (if (= ang1 ang2)
	     (setq value 'straight)
	   (progn
	     (setq angx (--- 'constrain-angle-value (- ang1 ang2)))
	     (cond 
	      ((> angx 180.0)
	       (setq value 'clock))
	      ((< angx 180.0)
	       (setq value 'counter-clock))
	      ((= angx 180.0)
	       (setq value 'whaaa?)) ; needs error message
	      ((= angx 0.0)
	       (setq value 'huh!))))) ; needs error message
	 (message (symbol-name value))
	 value)))
; fix winding by changing (- 360 (- ang1 angs)) to 
; (--- constrain (- ang1 ang2))
; and ( > ang 180) -> counter-clock (< ang 180) -> clock
; add error messages for whaaa? and huh1
    (constrain-angle-value ; maintain limit of 0-360 degrees
     (lambda(an-angle)
       (let ((value an-angle))
	 (while (< value 0)
	   (setq value (+ value 360)))
	 (while (> value 360)
	   (setq value (- value 360)))
	 value)))
    (rounded-method 
     (lambda ()
;       (message "rounded\n")
       (-- seg-d 'set-data 
	   (-- pt1 'get-x)(-- pt1 'get-y)
	   (-- pt3 'get-x)(-- pt3 'get-y))

       (-- seg-d 'to-line line-a)
       (-- (-- line-a 'get-angle) 'pt-to-line pt2 line-b)

       (-- seg-e 'set-data 
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt4 'get-x)(-- pt4 'get-y))

       (-- seg-e 'to-line line-c)
       (-- (-- line-c 'get-angle) 'pt-to-line pt3 line-d)


       (-- line-b 'xsect-line line-d pt-a)



       (-- seg-f 'set-data
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt-a 'get-x)(-- pt-a 'get-y))

      (-- seg-f 'mid-point ctrl-pt-1)

      (-- seg-g 'set-data
	   (-- pt-a 'get-x)
	   (-- pt-a 'get-y)
	   (-- pt3 'get-x)
	   (-- pt3 'get-y))
       (-- seg-g 'mid-point ctrl-pt-2)

       ))
    (straight-method 
     (lambda ()
;       (message "straight\n")
       (-- ctrl-pt-1 'set-x (-- pt2 'get-x))
       (-- ctrl-pt-1 'set-y (-- pt2 'get-y))
       (-- ctrl-pt-2 'set-x (-- pt3 'get-x))
       (-- ctrl-pt-2 'set-y (-- pt3 'get-y))))
    (start-rounded-method 
     (lambda ()
;       (message "start-rounded\n")
       (-- seg-d 'set-data 
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt4 'get-x)(-- pt4 'get-y))

       (-- seg-d 'to-line line-a)
       (-- (-- line-a 'get-angle) 'pt-to-line pt3 line-b)
       
       (-- (-- line-a 'get-angle) 'perpendicular ang-a)
       (-- ang-a 'pt-to-line pt2 line-c)
       (-- line-b 'xsect-line line-c pt-a)

       (-- seg-f 'set-data
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt-a 'get-x)(-- pt-a 'get-y))

      (-- seg-f 'mid-point ctrl-pt-1)
      (-- seg-g 'set-data
	   (-- pt3 'get-x)(-- pt3 'get-y)
	   (-- pt-a 'get-x)(-- pt-a 'get-y))
       (-- seg-g 'mid-point ctrl-pt-2)
       ))
    (rounded-end-method      
     (lambda ()
;       (message "rounded-end-point\n")
       (-- seg-d 'set-data 
	   (-- pt1 'get-x)(-- pt1 'get-y)
	   (-- pt3 'get-x)(-- pt3 'get-y))

       (-- seg-d 'to-line line-a)
       (-- (-- line-a 'get-angle) 'pt-to-line pt2 line-b)
       
       (-- (-- line-a 'get-angle) 'perpendicular ang-a)
       (-- ang-a 'pt-to-line pt3 line-c)
       (-- line-b 'xsect-line line-c pt-a)

       (-- seg-f 'set-data
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt-a 'get-x)(-- pt-a 'get-y))

      (-- seg-f 'mid-point ctrl-pt-1)
      (-- seg-g 'set-data
	   (-- pt3 'get-x)(-- pt3 'get-y)
	   (-- pt-a 'get-x)(-- pt-a 'get-y))
       (-- seg-g 'mid-point ctrl-pt-2)))

    (skewed-method      
     (lambda ()
;       (message "skewed\n")
       (-- seg-d 'set-data 
	   (-- pt1 'get-x)(-- pt1 'get-y)
	   (-- pt3 'get-x)(-- pt3 'get-y))
       (-- seg-d 'to-line line-a)

       (-- seg-e 'set-data
	   (-- pt2 'get-x)(-- pt2 'get-y)
	   (-- pt4 'get-x)(-- pt4 'get-y))
       (-- seg-e 'to-line line-b)

       (-- seg-f 'set-data 
	   (-- pt1 'get-x)(-- pt1 'get-y)
	   (-- pt4 'get-x)(-- pt4 'get-y))
       (-- seg-f 'to-line line-c)
       (-- seg-b 'to-line line-e)
       (-- line-e 'xsect-line line-c pt-a)

       (-- (-- (-- line-a 'get-angle) 'perpendicular ang-a) 'pt-to-line pt-a line-f)

       (-- line-b 'xsect-line line-f ctrl-pt-1)
       (-- line-a 'xsect-line line-f ctrl-pt-2)))

    (reset
     (lambda()
       (setq previous-2 nil)
       (setq previous nil)
       (setq current nil)
       (setq next nil)
       (-- line-a 're-init)
       (-- line-b 're-init)
       (-- line-c 're-init)
       (-- line-d 're-init)
       (-- line-e 're-init)
       (-- line-f 're-init)
       (-- seg-a 're-init)
       (-- seg-b 're-init)
       (-- seg-c 're-init)
       (-- seg-d 're-init)
       (-- seg-e 're-init)
       (-- seg-f 're-init)
       (-- seg-g 're-init)
       (-- seg-h 're-init)
       (-- seg-i 're-init)
       (-- pt-a 're-init)
       (-- pt-b 're-init)
       (-- pt-c 're-init)
       (-- ang-a 're-init)
       (-- ang-b 're-init)
       (-- ang-c 're-init)))))




(sos-class "path" "object"
  '((path-list ())(curve-assembler class))
  '(
    ;(add-to-path ;this does not appear to work or do anything even remotely coherent...
     ;(lambda (&rest data)
      ; (setq path-list
	;   (append path-list 
		;   (if (listp data)
		 ;      (element)
		  ;   (list element))))))
    (get-path-list
     (lambda ()
       path-list))
    (add-path
     (lambda (a-path-object)
       (setq path-list (append path-list (-- a-path-object 'get-path-list)))))
    (add-path-list
     (lambda (a-data-list)
       (setq path-list (append path-list a-data-list))))
    (add-element
     (lambda (an-element)
       (setq path-list (append path-list (list an-element)))))
    (add-element-to-front
     (lambda (an-element)
       (setq path-list (cons an-element path-list))))
    (remove-element-from-front
     (lambda ()
       (setq path-list (cdr path-list))))
    (clear-path
     (lambda ()
       (set 'path-list '())))
    (flatten-path
     (lambda ()
       (let ((path-data path-list)(output-list '()))
	 (while path-data
	   (let ((current-element (car path-data)))
	     (if (-- current-element 'is-a? "path")
		 (setq output-list (append output-list (-- current-element 'flatten-path)))
	       (setq output-list (append output-list (list current-element))))
	   (setq path-data (cdr path-data))))
	 output-list)))
    (render
     (lambda (&optional a-matrix)
       (let ((output-list (--- 'flatten-path))(output-file geo-output-file))
	 (while output-list
	   (let ((current-element (car output-list)))
	     (-- output-file 'write-string (-- current-element 'get-text (if a-matrix a-matrix))))
	   (setq output-list (cdr output-list))))))
    (output-text-list
     (lambda ()
       (let ((my-output-list (--- 'flatten-path))(output-text '()))
	 (while my-output-list
	   (setq output-text (append  output-text '("\n")))
	   (setq output-text (append  output-text (list (-- (car my-output-list) 'get-text))))
	   (setq my-output-list (cdr my-output-list)))
	 output-text)))
;    (shadow-copy ; is this a problem?
;     (lambda ()
;       (let ((new-instance (! class)))
;	 (-- new-instance 'set 'path-list path-list)
;	 new-instance)))
    (contribute-points
     (lambda (a-list)
       (let ((temp-path-list path-list))
	 (while temp-path-list
	   (setq a-list (-- (car temp-path-list) 'contribute-points a-list))
	   (setq temp-path-list (cdr temp-path-list))))
       a-list))
	 
    (smooth
     (lambda ()
       (let ((path-data path-list)
	     (curve-frame-list ())
	     (new-element nil)
	     (previous-2 nil)
	     (previous nil)
	     (current nil)
	     (next nil)
	     (first nil)
	     (last nil)
	     (shifted nil))
	 ;filter loop: build curve-frames and add to list
	 (while (setq new-element (car path-data))
	   (if (-- new-element 'is-a? "vertex-element")
	       (--- 'shift-vars new-element))
	   (if (-- new-element 'is-a? "paint")
	       (--- 'shift-vars nil))
	   (if current
	       (if (and shifted (-- current 'is-a? "curve-to"))
		   (--- 'build-curve-frame)))
	   (if (and shifted (-- new-element 'is-a? "paint"))
	       (progn
		 (--- 'close-curve-frame-path)
		 (--- 'reset-curve-frame-loop)))
	   (setq path-data (cdr path-data)));close of filter while loop
	 (--- 'shift-vars nil)
	 (if current
	     (if (-- current 'is-a? "curve-to")
		 (--- 'build-curve-frame)))
	 (--- 'close-curve-frame-path)
	 
;	 (print curve-frame-list)
;	 (debug)
	 (while curve-frame-list
	   (-- (--- 'get-class-var 'curve-assembler) 'fire (car curve-frame-list))
	   (setq curve-frame-list (cdr curve-frame-list))))
       self))

;private methods
     (build-curve-frame
      (lambda ()
;	 (message "build-curve-frame::line")
	(setq shifted nil)
	(setq last (cons previous-2 (cons previous (cons current (list next)))))
	(if (not first)
	    (setq first last)
	  (setq curve-frame-list (cons last curve-frame-list)))))
     (shift-vars ; only valid within smooth path analysis loop
      (lambda (value)
	(setq previous-2 previous)
	(setq previous current)
	(setq current next)
	(setq next value)
	(setq shifted t)))
    (reset-curve-frame-loop ; only valid within smooth path analysis loop
     (lambda ()
       (setq previous-2 nil)
       (setq previous nil)
       (setq current nil)
       (setq next nil)
       (setq first nil)
       (setq last nil)
       (setq shifted nil)))
    (close-curve-frame-path ; only valid within smooth path analysis loop
     (lambda ()
       ;if path is closed, update curve-frames for first and last
       (let ((first-test-value (car (cdr first)))(second-test-value (car (cdr (cdr last)))))
	 (if (and first-test-value second-test-value)
	     (if (--  first-test-value 'vertex-equiv? second-test-value)
		 (progn
		   (setq first (cons (car (cdr last)) (cdr first)))
		   (setq last 
			 (list 
			  (car last) 
			  (car (cdr last)) 
			  (car (cdr (cdr last))) 
			  (car (cdr (cdr first)))))
	     ;this line removes previously set last and adds back the new one
		   (setq curve-frame-list (cons last (cdr curve-frame-list))))))
	       (setq curve-frame-list (cons first curve-frame-list)))))))

(sos-set-class-var "path" 'curve-assembler (! "curve-assembler"))




; value supports string or number
(sos-class "value-object" "geo-metric"
  '((val nil))
  '((get-text
     (lambda ()
       (let ((string-value nil)(data-value val))
	 (if (stringp data-value)
	     (set 'string-value data-value))
	 (if (numberp data-value)
	     (set 'string-value (number-to-string data-value)))
	 (concat string-value " "))))))


;(sos-class "color" "value-object"
 ; nil
  ;'((set-color
   ;  (lambda (color-value)
    ;   (set 'val color-value)))))

;(defun grayscale-color (color-value)
 ; (let ((new-color (! "color")))
  ;  (-- new-color 'set-color color-value)
   ; new-color))





;path elements
(sos-class "path-element" "value-object"
  '((data-string nil)(command-string " ")(line-feed "\n"))
  '((get-text ; override value object get-text
     (lambda(&optional a-matrix)
       (if data-string
	   (concat data-string " " command-string line-feed)
	 (concat command-string line-feed))))
    (contribute-points
     (lambda (a-list)
       a-list))))


; the vertex-element acts like a 2d-point, but is not derived from it
; vertex elements are path elements that end somewhere.
(sos-class "vertex-element" "path-element"
  '((destination-point nil))
  '((init
    (lambda ()
      (if (not destination-point)
	  (setq destination-point (! "2d-point")))))
    (get-x 
     (lambda ()
       (-- destination-point 'get-x)))
    (get-y 
     (lambda ()
       (-- destination-point 'get-y)))
    (set-xy
     (lambda (x-val y-val)
       (-- destination-point 'set-xy x-val y-val)
       self))
    (set-x 
     (lambda (value)
       (-- destination-point 'set-x value)
       self))
    (set-y 
     (lambda (value)
       (-- destination-point 'set-y value)
       self))
    (vertex-equiv?
     (lambda (element)
       (if element
	   (if (-- element 'is-a? "vertex-element")
	       (if (and (= (--- 'get-x) (-- element 'get-x))
			(= (--- 'get-y) (-- element 'get-y)))
		   t
		 nil)
	     nil)
	 nil)))
    (contribute-points
     (lambda (a-list)
       (setq a-list (cons destination-point a-list))
       a-list))
    ))



(sos-class "move-to" "vertex-element"
  nil
  '((get-text before-ancestor
     (lambda (&optional a-matrix)
       (setq data-string (-- destination-point 'get-text (if a-matrix a-matrix)))
       (setq command-string "m")))))

(sos-class "line-to" "vertex-element"
  '((corner nil))
  '((get-text before-ancestor
     (lambda (&optional a-matrix)
       (setq data-string (-- destination-point 'get-text (if a-matrix a-matrix)))
       (if corner
	   (setq command-string "l")
	 (setq command-string "L"))))))
  
(sos-class "curve-to" "vertex-element"
  '((control-point-1 nil)(control-point-2 nil)(corner nil))
  '((init
     (lambda()
       (if (not control-point-1)
		(setq control-point-1 (! "2d-point")))
       (if (not control-point-2)
		(setq control-point-2 (! "2d-point")))
       (if (not destination-point)
		(setq destination-point (! "2d-point")))))
    (contribute-points
     (lambda (a-list)
       (setq a-list (cons destination-point a-list))
       (setq a-list (cons control-point-2 a-list))
       (setq a-list (cons control-point-1 a-list))
       a-list))
    (get-text before-ancestor
     (lambda (&optional a-matrix)
       (setq data-string
	     (concat 
	      (-- control-point-1 'get-text (if a-matrix a-matrix)) 
	      (-- control-point-2 'get-text (if a-matrix a-matrix)) 
	      (-- destination-point 'get-text (if a-matrix a-matrix))))
	(if corner
	    (setq command-string "c")
	  (setq command-string "C"))
	(message (concat "curve-to get-text data-string: "
			 data-string
			 "\n"))
))
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
       (-- destination-point 'set-y yval3)))))

(sos-class "paint" "path-element"
  '((close nil)(fill nil)(stroke nil)(clip nil))
  '((get-text before-ancestor
       (lambda (&optional a-matrix) ; this is a dummy reference... a-matrix has nothing to do with this (yet)
	 (let (
	       (closed? close)
	       (filled? fill)
	       (clipped? clip)
	       (stroked? stroke)
	       (opstring nil))
	   (if clipped?
	       (if closed?
		   (setq opstring "h\nW\nn\n")
		 (setq opstring "H\nW\n\nn"))	     
	     (if closed?
		 (if filled?
		     (if stroked?
			 (setq opstring "b")
		       (setq opstring "f"))
		   (if stroked?
		       (setq opstring "s")
		     (setq opstring "n")))
	       (if filled?
		   (if stroked
		       (setq opstring "B")
		     (setq opstring "F"))
		 (if stroked?
		     (setq opstring "S")
		   (setq opstring "N")))))
	   (setq command-string  opstring))))))

; clean up above code re. redundency of let (and rename vars to be consistent w/2d-path and frame)

; code used to successfully clip before clipping removed from paint.
;	   (if clipped
;	       (if closed
;		   (setq opstring "h\nW\nn\n")
;		 (setq opstring "H\nW\n\nn"))

;path building functions
; each take data and return new object instance


(defun path (&rest data)
  (if data
      (! "path" `(path-list ,data))
    (! "path")))

(defun moveto (xval yval)
  (! "move-to" `(destination-point ,(! "2d-point" xval yval))))

(defun lineto (xval yval &rest data)
  (if (memq 'corner data)
      (! "line-to" `(destination-point ,(! "2d-point" xval yval)) '(corner t))
  (! "line-to" `(destination-point ,(! "2d-point" xval yval)))))

(defun curveto (x1val y1val x2val y2val x3val y3val &rest data)
  (let ((temp-curve (! "curve-to")))
    (-- temp-curve 'set-values x1val y1val x2val y2val x3val y3val) 
    (if (memq 'corner data)
	(-- temp-curve 'set 'corner t))))

(defun curve (xval yval &rest data)
  (let ((temp-curve (! "curve-to")))
    (-- temp-curve 'set-values 0 0 0 0 xval yval)
    (if (memq 'corner data)
	(-- temp-curve 'set 'corner t))
    temp-curve))

(defun paint (&rest data)
  (let ((new-paint (! "paint")))
    (while data
	(-- new-paint 'set (car data) t)
	(setq data (cdr data)))
    new-paint))


;graphics state and grouping elements

(sos-class 
 "g-state-path-element" "path-element"
 '((symbol nil) ; note abstract class: does not work unless symbol is set
   (pop? nil))
 '((get-text 
    before-ancestor
    (lambda (&optional a-matrix)
      (let ((g-state (--- 'get-class-var 'graphics-state)))
	(if pop?
	    (progn
	      (-- g-state 'pop symbol)
	      (setq val (-- g-state 'get symbol)))
	  (-- g-state 'set symbol val))
	(if val
	    (setq data-string (number-to-string val)))
	)))))

(sos-class 
 "set-fill-color" "g-state-path-element"
 nil
 '((init 
    after-ancestor
    (lambda ()
      (setq symbol 'fill-color)
      (setq command-string "g")))))

(sos-class "set-stroke-color" "g-state-path-element"
  '((color nil))
 '((init 
    after-ancestor
    (lambda ()
      (setq symbol 'stroke-color)
      (setq command-string "G")))))

(sos-class "set-stroke-width" "g-state-path-element"
  '((value 1))
 '((init 
    after-ancestor
    (lambda ()
      (setq symbol 'stroke-width)
      (setq command-string "w")))))







(sos-class "group" "path-element"
  '((start? nil))
  '((get-text
     (lambda (&optional a-matrix)
       (if start?
	   (eval "u\n")
	 (eval "U\n"))))))

(sos-class "clip" "path-element"
  '((start? nil))
  '((get-text
     (lambda (&optional a-matrix)
       (if start?
	   (eval "q\n")
	 (eval "Q\n"))))))

(sos-class "compound-path" "path-element"
  '((start? nil))
  '((get-text
     (lambda (&optional a-matrix)
       (if start?
	 (eval "*u\n")
	 (eval "*U\n"))))))

; graphics state and grouping element building functions

(defun setlinewidth(num)
  (! "set-line-width" `(value ,num)))

(defun setfillcolor(acolor)
  (! "set-fill-color" `(color ,acolor)))

(defun setstrokecolor(acolor)
  (! "set-stroke-color" `(color ,acolor)))

(defun group(&rest data)
  (if (memq 'start data)
      (! "group" '(start? t))
    (! "group")))

(defun clip(&rest data)
  (if (memq 'start data)
      (! "clip" '(start? t))
    (! "clip")))


(defun compoundpath(&rest data)
  (if (memq 'start data)
      (! "compound-path" '(start? t))
    (! "compound-path")))




