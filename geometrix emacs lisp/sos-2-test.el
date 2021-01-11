; huh?

(setq prolog-file (! "input-file" '(filename "prolog.eps")))



; fix smooth

(progn
  (geo-reset-output-file)

(setq page (! "frame"))
(setq my-path (! "2d-path" '(stroked? t) '(stroke-width 2) '(stroke-color .5)))
(-- page 'add-element my-path)
(-- my-path 'add-element (-- (! "move-to") 'set-xy 25.0 25.0))
(-- my-path 'add-element (curve 50.0 25.0))
(-- my-path 'add-element (curve 50.0 50.0))
(-- my-path 'add-element (curve 25.0 50.0))
(-- my-path 'add-element (curve 25.0 25.0))

 (-- my-path 'smooth)
) ; smooth is currently broken (11/02/05)


(-- page 'render)

(geo-save-output-file)

(setq my-path-list (-- my-path 'get-path-list))
(setq sample-element (car (cdr my-path-list)))
(setq pt-1 (-- sample-element 'get 'control-point-1))
(setq pt-2 (-- sample-element 'get 'control-point-2))

(-- sample-element 'get-x)
(-- sample-element 'get-y)
(-- pt-1 'get-x)
(-- pt-1 'get-y)
(-- pt-2 'get-x)
(-- pt-2 'get-y)

(setq my-path-text
      (let ((output-text "")(path-list (-- my-path 'get-path-list)))
	(while path-list
	  (setq output-text (concat output-text (-- (car path-list) 'get-text)))
	  (message output-text)
	  (setq path-list (cdr path-list)))
	(setq output-text (concat output-text "\n"))
	output-text))

; alt data from above
(progn
  (geo-reset-output-file)

(setq page (! "frame"))
(setq my-path (! "2d-path" '(stroked? t) '(stroke-width 2) '(stroke-color .5)))
(-- page 'add-element my-path)
(-- my-path 'add-element (-- (! "move-to") 'set-xy 25.0 25.0))
(-- my-path 'add-element (curve 50.0 26.0))
(-- my-path 'add-element (curve 51.0 50.0))
(-- my-path 'add-element (curve 25.0 51.0))
(-- my-path 'add-element (curve 25.0 25.0))

 (-- my-path 'smooth)
) 

; gstate path elements

(setq gstate (sos-get-class-var "set-fill-color" 'graphics-state))
(-- gstate 'get 'fill-color)


(setq fill-1 (! "set-fill-color" '(val .5)))
(setq fill-2 (! "set-fill-color" '(pop? t)))

(-- fill-1 'get-text)
(-- fill-2 'get-text)


; test state and g-state

(setq g-state (! "g-state"))

(-- g-state 'get 'stroke-width)
(-- g-state 'get-list 'stroke-width)
(-- g-state 'set 'stroke-width 3)
(-- g-state 'pop 'stroke-width)

;error state
(-- g-state 'get 'stroke-width2)


(listp nil)

; test graphics-state

(setq g-state (! "graphics-state"))

(setq gs (sos-get-class-var "2d-space" 'graphics-state))

(-- gs 'get-stroke-color)

(-- gs 'push 'stroke-color .3)




; simplify paint overhaul test (before/after paths)

  ;build path
(progn
(setq page (! "frame"))
(setq my-path (! "2d-path" '(stroked? t) '(stroke-width 2) '(stroke-color .5)))
(-- page 'add-element my-path)
(-- my-path 'add-element (-- (! "move-to") 'set-xy 25.0 25.0))
(-- my-path 'add-element (-- (! "line-to") 'set-xy 50.0 25.0))
(-- my-path 'add-element (-- (! "line-to") 'set-xy 50.0 50.0))
(-- my-path 'add-element (-- (! "line-to") 'set-xy 25.0 50.0))
(-- my-path 'add-element (-- (! "line-to") 'set-xy 25.0 25.0))

; (-- my-path 'smooth) ; smooth is currently broken (11/02/05)
)

(-- page 'render)



(hash-to-let-list my-path)
(-- my-path 'get-path-list)
;	 (curve 0.0 5.0)
;	 (curve -5.0 0.0)
;	 (curve 0.0 -5.0)
;	 (curve 5.0 0.0)
;	 (paint 'close 'fill 'stroke))
;	'smooth))




; group and before/after path logic test

(progn
  (geo-reset-output-file)

; set up spigot
  (setq spigot-1 (! "spigot"))
  (-- spigot-1 'set-template-object
;       (-- 
	(let ((my-path (! "2d-path" )))
	  (-- my-path 'set-stroked? t)
	  (-- my-path 'set-filled? nil)
	  (-- my-path 'set-closed? nil)
	  (-- my-path 'add-element (moveto 5.0 0.0))
	  (-- my-path 'add-element (lineto 10 10))
	  my-path
	))
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
  (-- page 'set 'filled? nil)
  (-- page 'set 'stroked? nil)
  (-- page 'set 'clipped? nil)


  (setq frame-1 (! "frame"))
  (-- page 'add-elements frame-1)
  ; move to center of page
  (-- frame-1 'set-bounds-rect -100 -100 100 100)
  (-- frame-1 'set-custom-clipping-path 	
      (let ((clip-path (! "2d-path" )))
	  (-- clip-path 'add-element (moveto 0.0 100.0))
	  (-- clip-path 'add-element (lineto -100 0))
	  (-- clip-path 'add-element (lineto 0 -100))
	  (-- clip-path 'add-element (lineto 100 0))
	  (-- clip-path 'add-element (lineto 0 100))
	  (-- clip-path 'set-closed? t)
	  clip-path
	))

  (-- frame-1 'set 'filled? t)
  (-- frame-1 'set 'stroke-color .7)
  (-- frame-1 'set 'stroke-width 7)
  (-- frame-1 'set 'background-color .3)
  (-- frame-1 'set 'outline-stroke-color 0.0)
  (-- frame-1 'set 'outline-stroke-width 2)
  (-- frame-1 'set 'stroked? t)
  (-- frame-1 'set 'clipped? t)
  (-- frame-1 'move 306 396)
;  (-- frame-1 'set-clip nil)
  (-- spigot-1 'fire frame-1)
  (-- page 'render)

(geo-save-output-file)
)

(-- page 'render)
(-- frame-1 'render)

(setq page-contents (-- page 'get 'contents))
(setq frame-contents (-- frame-1 'get 'contents))
(setq temp-2d-path (car frame-contents))
(hash-to-let-list (-- temp-2d-path 'get 'after-path))




; dynamic-message test

(hash-to-let-list (sos-get-class-var "object" 'dynamic-message))




; can 'print' be used to build strings?

(setq a-string " ")

(setq another-string (prin1-to-string `("one two three: " '((1 2 3)))))




; do file objects work?
(setq test-file (! "output-file" 
		      '(filename "output.ai") 
		      `(prolog-file ,(! "input-file" '(filename "prolog.eps")))
		      `(trailer-file ,(! "input-file" '(filename "trailer.eps")))))

(setq temp-list (hash-to-let-list  test-file ))

(find-file "/Users/gregorkoomey/dev/emacs/current/eps/test.txt")

;


(setq test-point (! "2d-point"))
(-- test-point 'set-xy 5 10)



; test &rest


(defun blah (&rest data)
  (bloo data))

(defun bloo (&optional data)
  data)

(blah 1)




; test 2d-space

(setq space-1 (! "2d-space"))
(setq space-2 (! "2d-space"))

(-- space-1 'translate 10 100 space-1)
(setq space-3 (-- space-1 'concat-space space-2))

(setq temp-list (hash-to-let-list  space-3 ))
(-- space-1 'get 'my-matrix)
(-- space-2 'get 'my-matrix)
(-- space-3 'get 'my-matrix)


; test list as data to let-list

(setq test-data (1 2 3))

(let ((blah test-data))
test-data)

(setq temp-space (sos-get-class-var "2d-space" 'temp-space-2))

(setq temp-list (hash-to-let-list  temp-space))
(-- temp-space 'get 'class)

(-- temp-space 'get 'my-matrix)



; - test object class

(progn 
  (setq thing-a
	(sos-class 
	 "thing-a"  "object"
	 nil
	 '((init before-ancestor
	    (lambda () (message "0"))))))
  (setq thing-b
	(sos-class 
	 "thing-b"  "thing-a"
	 '((a 300) (b 5) (c 7))
	 '((init before-ancestor
	    (lambda () (--- 'pop-3)))
	   (pop
	    (lambda (an-object) 
	      (print `("pop: " ,a ,b ,c ))
	      (setq a (- a 1))
	      (setq b (+ b 1))
	      (print `("popb: " ,a ,b ,c ))
	      (-- an-object 'pop-2 self)))
	   (pop-3
	    (lambda ()
	      (print `("pop-3: " ,a ,b ,c )))))))
  (setq thing-c
	(sos-class 
	 "thing-c"  "thing-b"
	 '(d e f)
	 '((init before-ancestor
	    (lambda () (message "2")))
	   (pop-other
	    (lambda (an-object)
	      (-- an-object 'pop-2)))
	   (pop-2
	    (lambda (an-object)
	      (print `("pop-2"))
	      (-- an-object 'pop-3)))))))

(setq c-thing (! "thing-c"))

(-- c-thing 'set* 1 2 3 4 5 6)


(setq temp-list (hash-to-let-list  c-thing))


(-- test-out 'write-string "Gobbbldygooookk.....!!!!!\n")

(-- c-thing 'is-a? "thing-a")


(sos-class-to-list (gethash "2d-space" sos-classes))

; test &rest 
(defun var-test (&rest data)
  data)

(var-test 1)

(var-test 1 2 3)
(var-test)


; test ----
(progn 
  (setq thing-a
	(sos-class 
	 "thing-a"  "object"
	 nil
	 '((init before-ancestor
	    (lambda () (message "0"))))))
  (setq thing-b
	(sos-class 
	 "thing-b"  "thing-a"
	 '((a 300) (b 5) (c 7))
	 '((init before-ancestor
	    (lambda () (--- 'pop-3)))
	   (pop
	    (lambda (an-object) 
	      (print `("pop: " ,a ,b ,c ))
	      (setq a (- a 1))
	      (setq b (+ b 1))
	      (print `("popb: " ,a ,b ,c ))
	      (-- an-object 'pop-2 self)))
	   (pop-3
	    (lambda ()
	      (print `("pop-3: " ,a ,b ,c )))))))
  (setq thing-c
	(sos-class 
	 "thing-c"  "thing-b"
	 '(d e f)
	 '((init before-ancestor
	    (lambda () (message "2")))
	   (pop-other
	    (lambda (an-object)
	      (-- an-object 'pop-2)))
	   (pop-2
	    (lambda (an-object)
	      (print `("pop-2"))
	      (-- an-object 'pop-3)))))))
  (setq thing-d
	(sos-class 
	 "thing-d"  "thing-x"
	 '((a 5) (b 7))
	 '((init after-ancestor
	    (lambda () (message "3"))))))
  (setq thing-e
	(sos-class 
	 "thing-e" "thing-d"
	 nil
	 '((init before-ancestor
	    (lambda () (message "4"))))))

(setq temp-init (gethash 'init (gethash 'methods thing-a )))
(setq temp-init (gethash 'init (gethash 'methods thing-b )))


(symbolp nil)

(member nil '(2 3 4 5))


(setq temp-methods (hash-to-let-list (gethash 'class-vars thing-c )))
(sos-class-to-list thing-c)
(setq a-thing (! "thing-b" ))
(setq b-thing (! "thing-c" 5 nil 3))

(setq c-thing (! "thing-c"))

(-- c-thing 'set* 1 2 3 4 5 6)
(setq temp-list (hash-to-let-list  c-thing))




(setq temp-list (hash-to-let-list  a-thing))
(setq temp-list (hash-to-let-list  b-thing))



(sos-instance? a-thing)
(sos-class? a-thing)



(message "\n\nBEFORE POP TEST...\n")
(-- a-thing 'pop b-thing)
(message "\n\nAFTER POP TEST...\n")
nil





(progn  
(! "thing-a")
(! "thing-b")
(! "thing-c")
(! "thing-d")
(! "thing-e")
) 

  (setq thing-f
	(sos-class 
	 "thing-f"  "thing-a"
	 '((init before-ancestor
	    (lambda () (message "1"))))))
  (setq thing-g
	(sos-class 
	 "thing-g"  "thing-b"
	 '((d temp 2) (e class 4) (f 6))
	 '((init before-ancestor
	    (lambda () (message "2"))))))



;
(defvar test-params)

(defun test-frame-parameters (a-frame)
  (setq test-params (frame-parameters a-frame)))

(add-hook 'after-make-frame-functions 'test-frame-parameters)  

(remove-hook 'after-make-frame-functions 'test-frame-parameters)
   





;

(let nil (+ 3 5))

; test instance (relies on definitions below)
(setq an-instance (! "thing-d"))

(hash-to-let-list an-instance)
; ((class "thing-d") (c 7) (b 7) (a 5))




; test sos-class

(progn
  (setq sample-class-0
	(sos-class 
	 "thing-z"  "object"
	 nil
	 '((init
	    (lambda () (message "0"))))))
  (setq sample-class-1
	(sos-class 
	 "thing-a"  "object"
	 '((a 3) (b 5) (c 7))
	 '((init 
	    (lambda () (message "1"))))))
  (setq sample-class-2
	(sos-class 
	 "thing-b"  "thing-a"
	 '((d temp 2) (e class 4) (f 6))
	 '((init override
	    (lambda () (message "2"))))))
  (setq sample-class-3
	(sos-class 
	 "thing-c"  "thing-a"
	 '((a 5) (b 7))
	 '((init before-ancestor
	    (lambda () (message "3"))))))
  (setq sample-class-4
	(sos-class 
	 "thing-d" "thing-c"
	 nil
	 '((init after-ancestor
	    (lambda () (message "4")))))))
  
(! "thing-z")
(! "thing-a")
(! "thing-b")
(! "thing-c")
(! "thing-d")
 
(symbolp 'temp)

(setq class-0-data (sos-class-to-list sample-class-0))

(setq class-1-data (sos-class-to-list sample-class-1))
(setq class-2-data (sos-class-to-list sample-class-2))
(setq class-3-data (sos-class-to-list sample-class-3))
(setq class-4-data (sos-class-to-list sample-class-4))


(setq temp-class-method-list (assoc 'methods class-3-data))
(setq class-4-init-method (assoc 'init (assoc 'methods class-3-data)))

(setq class-4-init-method (assoc 'init (assoc 'methods class-4-data)))


(setq data-list nil)

(while class-1-data
  (let ((current-pair (car class-1-data))
	(current-hash nil))
    (setq current-hash (car (cdr current-pair)))
    (setq data-list (append (hash-to-let-list current-hash) data-list))
    (setq class-1-data (cdr class-1-data))))

(setq sample-classes (list sample-class-0 sample-class-1 sample-class-2 sample-class-3 sample-class-4))



(while sample-classes
  (setq sample-classes (cdr sample-classes)))



; How can I open formatted frames for system buffers, 
; *info*
; *backtrace*
; *messages*

; this doesn't work
; (find-file "*info*") 



; test hash-to-let-list with exceptions

(progn
  (setq hash-1 (make-hash-table))

  (puthash 'one 1 hash-1)
  (puthash 'two 2 hash-1)
  (puthash 'three 3 hash-1)
  (puthash 'four 4 hash-1)
  (puthash 'five 5 hash-1)


  (setq hash-2 (make-hash-table))

  (puthash 'six 6 hash-2)
  (puthash 'seven 7 hash-2)
  (puthash 'eight 8 hash-2)
  (puthash 'nine 9 hash-2)
  (puthash 'ten 10 hash-2)
)


(setq a-let-list (hash-to-let-list hash-2 'eight 'seven))

(setq b-let-list (hash-to-let-list hash-1 'eight 'seven))

(setq test-list (append a-let-list b-let-list))

(setq a-lambda-list (lambda () blah))
(setq a-sample-list '((lambda () blah)(lambda () blee)))

(listp a-lambda-list)
(functionp a-lambda-list)

(listp a-sample-list)
(functionp a-sample-list)

