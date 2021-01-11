; dynamic message object

(sos-class "dynamic-message" "object"
 '((message " ")
   (periods ".")
   (period-length 100))
 '((fire
    (lambda (a-message)
      (if (string-equal message a-message)
	  (progn
	    (setq periods (concat periods "."))
	    (if (> (length periods) period-length)
		(setq periods ".")))
	(progn
	  (setq message a-message)
	  (setq periods ".")))
      (message (concat message periods))))))



(setq test-message (! "dynamic-message"))

(setq limit 1000)

(while (> limit 0)
  (-- test-message 'fire "yowza")

  (setq limit (- limit 1)))
    







;research to try to deal with startup windows jumping around...
 
default-frame-alist 

(build-frame-parameters)

; prototype for persistent-frames.el
     
  



(progn
  (setq test-hash (make-hash-table))
  (puthash 'one 1 test-hash)
  (puthash 'two 2 test-hash)
  (puthash 'three 3 test-hash)
  (puthash 'four 4 test-hash)
  (puthash 'five 5 test-hash)
  (setq test-a-list (hash-to-a-list test-hash))
  (print `(,test-a-list)))

(setq temp-hash (gethash "Emacs - .emacs" all-open-frame-parameters))

(maphash
 (lambda (key value)
   (print `( ,key ,value)))
 temp-hash)
  
(save-open-frame-parameters)  
  
(setq all-open-frame-parameters (make-hash-table :test 'equal))

(setq temp-frames (visible-frame-list))  

(add-hook 'after-save-hook 'save-open-frame-parameters)
(remove-hook 'after-save-hook 'save-open-frame-parameters)

(add-hook 'kill-emacs-hook 'write-open-frame-parameters)

(defvar test-params)

(defun test-frame-parameters (a-frame)
  (setq test-params (frame-parameters a-frame)))

(add-hook 'after-make-frame-functions 'test-frame-parameters)  

(cons 'a-name (list '(key . 3))) 

(remove-hook 'after-make-frame-functions 'test-frame-parameters)
   
(maphash
 (lambda (key value) 
   (print `( ,key 
	     "\n " 
	     ,(maphash 
	       (lambda (key2 value2)
		 (print `("     " ,key2 " " ,value2 "\n")))
	       value))))
 all-open-frame-parameters)
 
  
(maphash
 (lambda (key value) 
   (print `( ,key ,value) 
	     ))
 all-open-frame-parameters)


(clrhash all-open-frame-parameters)


     ; save hook

(remove-hook 'after-save-hook
	  (lambda ()
	    (message "Yowza")))

(setq whaaa (current-frame-configuration))

mode-line-format

; test aquamacs frame stuff (for robust-frames.el)

(setq temp-frames (visible-frame-list))

(setq temp-a-list (frame-parameters (car temp-frames)))

     ; the following does not work for some reason
(let ((frame-list temp-frames)(current-frame nil))
  (while frame-list
    (setq current-frame (car frame-list))
    (if (string= "Emacs - test.el" (frame-parameter current-frame 'name))
	(setq temp-frame current-frame))
    (setq frame-list (cdr frame-list))
    temp-frame))


(defun winnow-frame-param-list (a-list)
  (let ((new-list nil))
      (setq new-list (cons (assoc 'top a-list) new-list))
      (setq new-list (cons (assoc 'left a-list) new-list))
      (setq new-list (cons (assoc 'width a-list) new-list))
      (setq new-list (cons (assoc 'height a-list) new-list))
    new-list))
  
(setq blah (winnow-frame-param-list temp-a-list))
(setf (cdr (car (cdr blah))) 150)

(modify-frame-parameters (car temp-frames) blah)

(equal 'top (car (assoc 'top blah)))

(assoc 'top temp-a-list)


(frame-parameter (car temp-frames) 'name)

(listp '(3 . 0))

; test perspective more
(progn
  (default-graphics-state)

  (setq p-frame (! "perspective-frame"))

  (-- p-frame 'set-metaframe nil nil 200)


  (let ((y-val 0))
    (while (< y-val 700)
      (-- p-frame 'add-element
	  (2d-path 
	   (moveto* 100 y-val -100)
	   (lineto* 100 y-val 500)
	   (lineto* 500 y-val 500)
	   (lineto* 500 y-val -100)
	   (lineto* 100 y-val -100)
	   (paint  'stroke)))
      (setq y-val (+ 30 y-val))))
  (-- p-frame 'render*)

  (save-output-file)
)


; test perspective frame assembly
(progn

  (setq test-p-frame (instance "perspective-frame"))


  (setq test-path 
	(2d-path 
	 (moveto* 10 10 10)
	 (lineto* 10 10 300)
	 (lineto* 300 10 300)
	 (lineto* 300 10 10)
	 (lineto* 10 10 10)
	 (paint  'stroke)))


  (-- test-p-frame 'add-element test-path))


(-- test-p-frame 'render)
(-- test-p-frame 'render*)


  (setq meta-test (-- test-p-frame 'get 'metaframe))




;test frame alpha


(progn
  (setq frame-1 (instance "frame"))

  (-- frame-1 'move 306 396)
  (-- frame-1 'set-bounds 50 50 100 100)
;  (-- frame-1 'set-clip nil)
  (setq path-1 (2d-path (moveto 0 0) (lineto 300 50) (paint 'stroke)))
  (-- frame-1 'add-element path-1)
)

(setq a-path-list (-- path-1 'get-path-list))


(let ((current-string " "))
  (while a-path-list
    (let ((current-element (car a-path-list)))
      (if (-- current-element 'is-a? "vertex-element")
	(setq 
	 current-string 
	 (concat 
	  current-string 
	  (number-to-string 
	   (-- current-element 'get-x)) 
	  " "
	  (number-to-string 
	   (-- current-element 'get-y))
	  " "))))
	  (setq a-path-list (cdr a-path-list)))
  current-string)

(-- frame-1 'render)



(progn
; set up spigot
  (setq spigot-1 (! "spigot"))
  (-- spigot-1 'set-template-object
;       (-- 
	(2d-path 
	 (moveto 5.0 0.0)
	 (lineto 10 10)
	 (paint 'stroke)))
;	 (curve 0.0 5.0)
;	 (curve -5.0 0.0)
;	 (curve 0.0 -5.0)
;	 (curve 5.0 0.0)
;	 (paint 'close 'fill 'stroke))
;	'smooth))
  (-- spigot-1 'set-size-range .5 3)
  (-- spigot-1 'set-ang-range 0 360)
  (-- spigot-1 'set 'ticks 200)
  (-- spigot-1 'set 'bounds-range 200)
;set up page  
  (setq page (! "frame"))
  (setq frame-1 (! "frame"))
  (-- page 'add-elements frame-1)
  ; move to center of page
  (-- frame-1 'set-bounds -100 -100 100 100)
  (-- frame-1 'set-draw-outline t)
  (-- frame-1 'move 306 396)
;  (-- frame-1 'set-clip nil)
)

(-- spigot-1 'fire frame-1)
(-- page 'render)
(-- frame-1 'render)



(setq huh (car (-- frame-1 'get 'contents)))

(setq huh-path (-- huh 'get 'my-path))

(-- huh-path 'get 'path-list)








;test rectangle

(setq rect-1 (instance "rectangle"))

(-- rect-1 'set-data 0 0 100 100)

(print (-- (-- rect-1 'path) 'output-text-list))

; test spatial concatenation
(progn
  (setq trans (instance "2d-space"))
  (setq rot (instance "2d-space"))
  (setq scale (instance "2d-space"))
  (setq trans-rot (instance "2d-space"))
  (setq rot-trans (instance "2d-space"))
  (setq trans-scale (instance "2d-space"))
  (setq scale-trans (instance "2d-space"))

  (setq trans-trans-scale (instance "2d-space"))


  (-- trans 'translate 100 100 trans)
  (-- rot 'rotate 45 rot)
  (-- scale 'scale 1.5 1.5 scale)
  (-- trans 'concat-space rot trans-rot)
  (-- rot 'concat-space trans rot-trans)
  (-- trans 'concat-space scale trans-scale)
  (-- scale 'concat-space trans scale-trans)

  (-- trans 'concat-space trans trans-trans-scale)
  (-- trans-trans-scale 'concat-space scale trans-trans-scale)
)



(setq point-1 (instance "2d-point"))
(-- point-1 'set-x 0)
(-- point-1 'set-y 5)

(-- point-1 'get-text trans)
(-- point-1 'get-text rot)
(-- point-1 'get-text scale)
(-- point-1 'get-text trans-rot)
(-- point-1 'get-text rot-trans)
(-- point-1 'get-text trans-scale)
(-- point-1 'get-text scale-trans)
(-- point-1 'get-text trans-trans-scale)


;test first spigot

(progn
; set up spigot
  (setq spigot-1 (instance "spigot"))
  (-- spigot-1 'set-template-object
       (2d-path 
	(moveto 0 0)
	(lineto 0 5)
	(paint 'stroke)))
  (-- spigot-1 'set-size-range .5 3)
  (-- spigot-1 'set-ang-range 0 360)
  (-- spigot-1 'set 'ticks 200)
  (-- spigot-1 'set 'bounds-range 200)
;set up page  
  (setq page (instance "frame"))
  (setq frame-1 (instance "frame"))
  (-- page 'add-elements frame-1)
  ; move to center of page
  (-- frame-1 'move 306 396))

(-- spigot-1 'fire frame-1)

(-- page 'size .3)

(-- page 'render)
(-- frame-1 'render)

;test shadow path


(setq spot
      (--
       (2d-path 
	(moveto 0 5)
	(curve 5 0)
	(curve 0 -5)
	(curve -5 0)
	(curve 0 5)
	(paint 'close 'fill 'stroke))
       'smooth))




(let ((iteration 0))
  (while (< iteration 100) 
    (-- frame-1 'add-elements
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300))
	(-- (-- spot 'shadow-copy) 'move-to-point (bell-random-point 300)))
    (setq iteration (+ iteration 1))))

(-- page 'render)

; test space-set ops: spin and size

(progn
  (setq page (instance "frame"))
  (setq frame-1 (instance "frame"))
  (-- page 'add-elements frame-1)
  ; move to center of page
  (-- frame-1 'move 306 396)
  (-- frame-1 'add-elements
      (--
       (--
	(2d-path 
	 (moveto 0 0)
	 (lineto 10 0)
	 (paint  'stroke))
	'move 25 25)
       'spin 10)
      (--
       (--
	(2d-path 
	 (moveto 0 0)
	 (lineto 10 0)
	 (paint  'stroke))
	'move 50 50)
       'spin 40)
      (--
       (--
	(2d-path 
	 (moveto 0 0)
	 (lineto 10 0)
	 (paint  'stroke))
	'move 75 75)
       'size 2)
      (--
       (--
	(2d-path 
	 (moveto 0 0)
	 (lineto 10 0)
	 (paint  'stroke))
	'move 100 100)
       'size 6)
      ))


(-- page 'render)
(-- frame-1 'render)

; test frame

(setq page (instance "frame"))

(setq frame-1 (instance "frame"))
(setq frame-2 (instance "frame"))

(-- page 'add-elements frame-1 frame-2)

(-- frame-1 'add-elements
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 25 30)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 100 200)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 50 40)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 75 300))


(-- frame-2 'add-elements
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 25 30)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 100 200)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 50 40)
    (--
     (--
      (2d-path 
       (moveto 0 5)
       (curve 5 0)
       (curve 0 -5)
       (curve -5 0)
       (curve 0 5)
       (paint 'close 'fill 'stroke))
      'smooth) 'move 75 300))
    
(-- frame-2 'move 200 300)



(-- page 'render)


; test 2d-path 

(setq test-path
      (--
       (path 
	(moveto 0 5)
	(curve 5 0)
	(curve 0 -5)
	(curve -5 0)
	(curve 0 5)
	(paint 'close 'fill 'stroke))
       'smooth))


(setq test-2d-path
      (--
       (2d-path 
	(moveto 0 5)
	(curve 5 0)
	(curve 0 -5)
	(curve -5 0)
	(curve 0 5)
	(paint 'close 'fill 'stroke))
       'smooth))


(-- test-2d-path 'get 'class)
(-- test-2d-path 'get-class-var 'accessors)
(-- test-2d-path 'get-class-var 'class-path)
(-- test-path 'get-class-var 'time-stamp)
(-- test-2d-path 'rotate 1 test-2d-path)
(-- test-2d-path 'move 25 30)
(-- test-2d-path 'render (instance "2d-space"))

(setq test-space (instance "2d-space"))

(setq temp (gethash "2d-space" classes))


(hash-to-let-list (gethash 'class-vars (gethash "2d-space" classes)))
(hash-to-let-list (gethash 'class-vars (gethash "2d-path" classes)))



(-- test-2d-path 'output-text-list)
(-- test-path 'output-text-list)


;clip float function

(setq a-num 77999.1415967892345987277)

(clip-float a-num)



; test mod of 2d-point::get-text to accept matrix
(setq test-point (instance "2d-point" '(x 10) '(y 0)))

(setq test-space (instance "2d-space"))
(setq test-space-2 (-- test-space 'translate 5 10))

(-- test-point 'get-text test-space-2)


; rotation test
(let ((ang-val 0))
  (while (< ang-val 361)
    (-- test-out 'write-line `(,(number-to-string ang-val) ":" ,(-- (-- (-- test-space 'rotate ang-val) 'transform test-point) 'output-text)))
    (setq ang-val (+ ang-val 1))))

;concat test
(let ((ang-val 1))
  (while (< ang-val 361)
    (-- test-out 'write-line 
	`(,(number-to-string ang-val) 
	  ":" 
	  ,(-- 
	    (-- 
	     (-- test-space 'rotate 1 test-space) 
	     'transform test-point) 
	    'output-text)))
    (setq ang-val (+ ang-val 1))))



;test 2d-space-matrix object

(setq test-space (instance "2d-space"))
(-- test-space 'get 'my-matrix)

(setq test-point (instance "2d-point" '(x 5) '(y 5)))
(list (-- test-point 'get-x) (-- test-point 'get-y))

(setq test-space-2 (-- test-space 'translate 5 10))
(-- test-space-2 'get 'my-matrix)
(setq test-point-2 (-- test-space-2 'transform test-point))
(list (-- test-point-2 'get-x) (-- test-point-2 'get-y))


(setq test-space-3 (-- test-space 'scale 5 5))
(-- test-space-3 'get 'my-matrix)
(setq test-point-3 (-- test-space-3 'transform test-point))
(list (-- test-point-3 'get-x) (-- test-point-3 'get-y))

(setq test-space-4 (-- test-space 'rotate 270))
(-- test-space-4 'get 'my-matrix)
(setq test-point-4 (-- test-space-4 'transform test-point))
(list (-- test-point-4 'get-x) (-- test-point-4 'get-y))

(expand-sparse-matrix (-- test-space-4 'get 'my-matrix) 3)



; test crazy setf/path data strategy for matrix.el
(progn
  (setq my-hash (make-hash-table))
  (puthash 'a '(quote (car (last (car (car (last (car temp-matrix))))))) my-hash)
  (puthash 'b '(quote (car (last (car (car (last (nth 1 temp-matrix))))))) my-hash)
  (puthash 'c '(quote (car (last (car (car (last (nth 2 temp-matrix))))))) my-hash)
  (puthash 'd '(quote (car (last (nth 1 (car (last (car temp-matrix))))))) my-hash)
  (puthash 'e '(quote (car (last (nth 1 (car (last (nth 1 temp-matrix))))))) my-hash)
  (puthash 'f '(quote (car (last (nth 1 (car (last (nth 2 temp-matrix))))))) my-hash)
)

(gethash 'c my-hash)

(setq temp-matrix (make-identity-matrix 3))



(eval 
 `(let ,(hash-to-let-list my-hash)
    (eval `(setf ,c 6))
	(eval c)))

(hash-to-let-list my-hash)


(setq test-line (instance "line"))

(last (car (last (car temp-matrix))))




; test matrix code from winston and horn LISP

(sparse-scale-v 2 '((1 1.2) (3 3.4) (6 -6.7)))

(sparse-dot-product '((1 2) (3 3) (6 4)) '((1 1) (6 3)))

(sparse-v-plus '((1 2) (3 3) (6 4)) '((1 1) (6 3)))

(sparse-m-times '((1 ((2 2))) (2 ((1 1))) (3 ((3 1))))
		'((1 ((2 3))) (2 ((3 3))) (3 ((1 4)))))


(setq matrix-1 '((1 ((2 3))) (2 ((3 3))) (3 ((1 4)))))

(setq expanded-matrix-1 (expand-sparse-matrix matrix-1 3))

(setq matrix-2 (copy-tree matrix-1))

(setf (car (car (cdr matrix-2))) 'x) 

old-row-list

(expand-sparse-matrix (compact-sparse-matrix (make-identity-matrix 3)) 3)

(setf test-identity (make-identity-matrix 4))

; test selfstack depth

(def-class "self-stack-tester" "object"
  '((local-count 100))
  '((count 0))
  `(
    (init
     (lambda ()
       (let 
	   ((my-count 
	     (gethash 
	      'count (gethash 
		      'class-vars 
		      (gethash 
		       (gethash 'class self) 
		       classes)))))
	 (if (< my-count 10)
	     (progn
	       (setq local-count (- local-count 1))
	       (puthash 
		'count 
		(+ my-count 1) 
		(gethash 'class-vars 
			 (gethash (gethash 'class self) classes)))
	       (-- (instance "self-stack-tester") 'fire))))))
    (fire
     (lambda ()
       (let 
	   ((my-count 
	     (gethash 
	      'count (gethash 
		      'class-vars 
		      (gethash 
		       (gethash 'class self) 
		       classes)))))
	       (message 
		(concat 
		 "instance count: "
		 (number-to-string my-count) 
		 "  local count: " 
		 (number-to-string local-count)))
       )))))



	 
(instance "self-stack-tester")


;test curve smooth and curve-assembler


(setq test-prolog (instance "input-file" '(filename "prolog.eps")))
(setq test-trailer (instance "input-file" '(filename "trailer.eps")))
(setq test-doc 
      (instance "output-file" 
		'(filename "yowza.ai")
		`(prolog-file ,test-prolog)
		`(trailer-file ,test-trailer )))  


(length selfstack)

(setq selfstack '())

(instance "set-fill-color")

(default-graphics-state)

(setq bob 5)
(eval (quote bob))

;'smooth)
(setq test-path
; (-- 
(path
  (moveto 200 100)
  (curve 100 300)
  (curve 100 500)
  (curve 300 300)
  (curve 600 500)
  (curve 700 300)
  (curve 300 50)
;  (curve 100 100)
;  (setfillcolor 0)
  (paint 'stroke)))
;  (paint 'close 'fill 'stroke)))

(-- test-path 'smooth)

(-- test-path 'render-path)

(save-output-file)





(setq test-path
; (-- 
(path
  (moveto 100 100)
  (curve 0 300)
  (curve 100 500)
  (curve 300 500)
  (curve 400 300)
  (curve 300 100)
  (curve 100 100)
;  (setfillcolor 0)
  (paint 'close 'fill 'stroke)))


(-- test-out 'write-line (-- test-path 'output-text-list))
(-- test-path 'flatten-path)



(hash-to-let-list test-seg1)




(setq test-seg1 (line-segment 100 50 50 -1000))

(-- (-- test-seg1 'mid-point) 'get-text)



(let ((test-ang (instance "angle"))(my-angle 0)(temp-slope nil))
  (while (< my-angle 360)
    (-- test-ang 'set-ang my-angle)
    (setq temp-slope (-- test-ang 'get-slope))
    (-- test-out 'write-line
	`("Angle: "
	  ,(number-to-string my-angle)
	  "slope: "
	  ,(if temp-slope
	       (number-to-string temp-slope)
	     "nil ")))
    (setq my-angle (+ 1 my-angle))))
	
	  




;xsect test

(setq test-point (2d-point 100.0 10.0))
(setq test-line (line 45 0))
(setq test-line (-- test-point 'angle-to-line 45))
;(setq test-line (line 90 nil 5))
(setq test-line2 (line 135 5))

(setq temp (instance "move-to"))
(-- temp 'get-x)

(setq test-seg1 (line-segment 100 10 -10 0))
(-- test-seg1 'get-class-var 'class-path)
(-- test-seg1 'is-a? "line")
(gethash "object" classes)

(gethash "line-segment" classes)
(setq test-seg2 (line-segment 2 0 0 10))

(-- test-seg1 'output-text)
(setq test-line (-- test-seg1 'to-line))
(-- test-line 'output-text)


(-- (-- (-- test-seg1 'get-line) 'xsect-line (-- test-seg2 'get-line)) 'get-text)


(-- (-- test-line 'xsect-line test-line2) 'output-text)

(-- (-- test-line 'get-angle) 'get-angle)
(-- test-line 'get-m)
(-- test-line 'get 'b)
(-- test-line 'get 'x)


; angle:pt-to-line test
(setq test-angle (instance "angle"))
(setq test-point (2d-point 100.0 10.0))
(-- test-angle 'set-angle 260)
(-- test-angle 'get-angle)
(-- test-angle 'get-slope)
(-- (-- test-angle 'pt-to-line test-point) 'output-text)


;xsect test
(setq line1 (line 0 5))

(setq seg (line-segment 0 -5 -10 10))

(defun xsect-test ()
    (-- test-out 'write-line
	`(
	  "Xval:"
	  ,(number-to-string xval)
	  "Yval:"
	  ,(number-to-string yval)
	  " pt:"
	  ,(let ((test-point (-- line1 'xsect-line line2))) 
	       (if test-point
		   (-- test-point 'get-text)
	     " nil"))
	  )))


(let
    ((xval -10)(yval 10)(line2 nil))
  (while (< xval 11)
    (-- (-- seg 'get 'end) 'set-x xval)
    (setq line2 (-- seg 'to-line))
    (xsect-test)
    (setq xval (+ xval 1)))
  (setq yval (-- (-- seg 'get 'end) 'get-y))
  (while (> yval -11)
    (-- (-- seg 'get 'end) 'set-y yval)
    (setq line2 (-- seg 'to-line))
    (xsect-test)
    (setq yval (- yval 1)))
  (while (> xval -11)
    (-- (-- seg 'get 'end) 'set-x xval)
    (setq line2 (-- seg 'to-line))
    (xsect-test)
    (setq xval (- xval 1)))
  (while (< yval 11)
    (-- (-- seg 'get 'end) 'set-y yval)
    (setq line2 (-- seg 'to-line))
    (xsect-test)
    (setq yval (+ yval 1))))

(setq test-seg (line-segment 5 5 6 73))
(setq test-line3 (-- test-seg 'to-line))

(-- test-line3 'output-text)


(setq test-angle (instance "angle"))
(setq test-point (2d-point 1 1))

(let ((temp-angle 0))
  (while (< temp-angle 360)
    (-- test-angle 'set-ang temp-angle)
    (-- test-out 'write-line
	`(
	  ,(-- (-- test-angle 'pt-to-line test-point) 'output-text)
	  "\n"
	  ))
    (set 'temp-angle (+ temp-angle 1))))




(let ((temp-angle 0))
  (while (< temp-angle 360)
    (-- test-angle 'set-angle temp-angle)
    (-- test-out 'write-line
	`(
	  "angle: "
	  ,(number-to-string (-- test-angle 'get-angle))
	  " slope: "
	  ,(let ((temp-slope (-- test-angle 'get-slope)))
	     (if temp-slope
		    (number-to-string temp-slope)
	       "nil "))))
    (set 'temp-angle (+ temp-angle 1))))


(setq test-segment (line-segment 25 30 5 25))

(-- (-- test-segment 'get 'end) 'get-x)

(setq test-line (-- test-segment 'to-line))

(-- test-line 'get-ang)



(let ((x1 0) (y1 0) (x2 5) (y2 0))
  (while (< y2 5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))
    (set 'y2 (+ y2 1))) 
  (while (> x2 -5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))

    (set 'x2 (- x2 1))) 
  (while (> y2 -5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))
    (set 'y2 (- y2 1))) 
  (while (< x2 5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment  'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))
    (set 'x2 (+ x2 1))) 
  (while (< y2 0)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end ) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))
    (set 'y2 (+ y2 1))))







(let ((x1 0) (y1 0) (x2 5) (y2 0))
  (while (< y2 5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))
    (set 'y2 (+ y2 1)))
  (while (> x2 -5)
    (-- test-segment 'set-data x1 y1 x2 y2)
    (setq test-line (-- test-segment 'get-line))
    (-- test-out 'write-line
	`( 
	 ,(-- (-- test-segment 'get 'end) 'get-text) 
	 ,(-- (-- test-line 'get 'm) 'get-text)
	 "\n"))

    (set 'x2 (- x2 1))) ) 

(eq "astring" "astring")
(equal "astring" "astring")

;test stuff
(def-class "other" "object" 
'((x 0) (y 0) (z 0)) 
nil 
'((get bind-method)
  (blah 
   (lambda ()
     z))))  

;(gethash 'z (gethash 'instance-vars (gethash "other" classes))) 
;(eval (gethash 'bob (gethash 'methods (gethash "object" classes)))) 


(gethash 'class-path (gethash 'class-vars (gethash "object" classes)))
(gethash 'class-path (gethash 'class-vars (gethash "other" classes)))
(gethash 'get (gethash 'methods (gethash "object" classes)))
(gethash 'get (gethash 'methods (gethash "other" classes)))
(gethash 'set (gethash 'methods (gethash "other" classes)))

(setq badbob (instance "other"))
(setq badbob (instance "other" '(z 7) '(x 5)))
(--  badbob 'class-name)
(-- badbob 'get 'x)
(-- badbob 'get 'y)
(-- badbob 'get 'z)
(-- badbob 'blah)

(-- badbob 'set 'z 35)
(-- badbob 'get-class-var 'time-stamp)

(-- badbob 'set* '((x 17)(z 32)(y 54))); 

(maphash 
 (lambda (key value)
   (message key))
   classes)
(setq bigbob (instance "other" '(z 7) '(x 5)))
(setq badbob (instance "other" '(z 7) '(x 5)))

(-- bigbob 'equiv? badbob)


(def-class "nother" "object" 
  `((start ,(2d-point 1 2))
    (end ,(2d-point 3 4))) 
  nil 
  '((get bind-method)))  

(setq bigbob (instance "nother"))
(setq badbob (instance "nother"))
(setq lilbob (instance "nother" `(start ,(2d-point 6 7))))

(-- (-- lilbob 'get 'start) 'get-text)


(-- bigbob 'equiv? lilbob)


;(funcall (lambda () (print "blahlll!")))



 ;(let ((class-path-length 5))
	;	   (while (> class-path-length 1)
		;   (if (time-less-p class-time 
			;	    (gethash 'time-stamp (gethash 'class-vars (car class-path))))
	;	       (progn
		;;	 (cons value mod-list)
			; (setq class-path-length (- class-path-length 1))))))



;;test file object
(setq testfile (instance "buffer-file" '(filename "bubba.ai")))
(--  testfile 'write-string "wholy shit sherlock????!!!  What the fuck!!!!?")
(--  testfile 'write-line '("wholy" "shit" "sherlock????!!!"))
(-- testfile 'save )
(--  testfile 'clear)
(--  testfile 'string-value)

(setq testfile2 (instance "input-file" '(filename "prolog.eps")))


(--  testfile2 'string-value)
(--  testfile 'write-string (--  testfile2 'string-value))


(setq test-prolog (instance "input-file" '(filename "prolog.eps")))
(setq test-trailer (instance "input-file" '(filename "trailer.eps")))

(setq test-buffer (instance "buffer-file" '(filename "buff.eps")))



(-- test-prolog 'class-name)



(--  test-doc 'write-line '("wholy" "shit" "sherlock????!!!"))
(--  test-doc 'write-string "wholy shit sherlock????!!!  What the fuck!!!!?")
(-- test-doc 'save )



(setq test-value (instance "value-object" '(val 73)))
(-- test-value 'get-text)
(-- test-value 'set 'val "blah!")
(-- test-value 'set 'val 4)
(-- test-value 'get 'val)

(setq test-point (2d-point 5 7))
(-- test-point 'get-text) 



(-- (gethash 'line-width graphics-state) 'get 'val)


(-- 
  (path
  (moveto 140 140)
  (lineto 140 160)
  (lineto 160 160)
  (lineto 160 140)
  (lineto 140 140)
 

  (paint 'close 'fill 'stroke))
 'render-path)

(setq mypath (instance "path"))
(setq mypath 
      (instance 
       "path" 
       `(path-list
	  ,(moveto 140 140)
	  ,(lineto 140 160)
	  ,(lineto 160 160)
	  ,(lineto 160 140)
	  ,(lineto 140 140))
	 (paint 'close 'fill 'stroke)))

(setq blah '(1 3 4 5))
(setq blee `(,blah))











(-- (gethash 'eps-file graphics-state) 'save)


(defun some-function ()
  (funcall bob))

(let ((bob (lambda () (message "hey?"))))
  (some-function))

; angle

(setq myangle (instance "angle"))

(setq myangle2 (instance "angle"))

(-- myangle 'set-angle 90)
(-- myangle2 'set-angle 270)
(-- myangle2 'set-angle 90)
(-- myangle 'equiv? myangle2)
(-- myangle 'equal? myangle2)


(-- myangle 'set-angle 120)
(-- myangle 'set-angle-from-radian 3)
(-- myangle 'get-angle)
(-- myangle 'get-radians)

(setq perp-angle (-- myangle 'perpendicular))
(-- perp-angle 'get-angle)



(setq temp (-- myangle 'sin))
(-- myangle 'cos)
(-- myangle 'tan)

(-- myangle 'set-angle 120)
(-- perp-angle 'asin-set (-- myangle 'sin))

(-- myangle 'set-angle 350)
(-- perp-angle 'acos-set (-- myangle 'cos))

(-- myangle 'set-angle 160)
(-- perp-angle 'atan-set (-- myangle 'tan))

(def-class "self-test" "object"
  '((what 73))
  nil
  '((blah 
     (lambda()
       (-- self 'get 'what)))))

(setq huh (instance "self-test"))

(-- huh 'blah)





;test to print all class names
;(maphash (lambda (key value) (print key)) classes)
; and class-path length
;(maphash (lambda (key value) (print key) (print (length (gethash 'class-path value)))) classes)





;nested function query
; THIS TEST FAILED MISERABLY
;(setq bob 0)
;
;(let ((setq nil))
;  (progn
;    (defun setq (symbol value)
;	     (set (quote symbol) value)
;	     (message "blah"))
;    (setq bob 1)))

;THIS FAILED TOO!!! 
;(defadvice setq (after sos-method-data-integrity (sym value))
;  (-- self 'set sym value))


  
