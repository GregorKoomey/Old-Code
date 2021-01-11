; objects to algorithmically generate graphx

; relies on sos and geometrix

(sos-class "spigot" "object"
  '((ticks 10)
    (start-size 1)
    (end-size 1)
    (size-iteration 0)   
    (start-ang-val 0)
    (start-ang-val 0)
    (ang-iteration 0)   
    (bounds-range 300)
    (template-object nil))
  '((re-init
     (lambda ()
       (setq size-iteration 0)
       (setq start-size 1)
       (setq end-size 1)
       (setq ang-iteration 0)
       (setq start-ang 0)
       (setq end-ang 0)
))
    (synchronize
     (lambda ()
       (if (not (= start-size end-size))
	   (setq size-iteration (/ (- end-size start-size) ticks))
	 (setq size-iteration 0))
       (if (not (= start-ang-val end-ang-val))
	   (setq ang-iteration (/ (- end-ang-val start-ang-val) ticks))
	 (setq ang-iteration 0))))
	    
    (set-size-range
     (lambda (start-val end-val)
       (setq start-size start-val)
       (setq end-size end-val)
       self))
    (set-ang-range
     (lambda (start-val end-val)
       (setq start-ang-val start-val)
       (setq end-ang-val end-val)
       self))
    (get-new-object
     (lambda ()
       (-- template-object 'shallow-copy)))
    (get-new-location
     (lambda ()
       (bell-random-point bounds-range)))
    (set-template-object
     (lambda (a-2d-path)
       (setq template-object a-2d-path)))

    (fire
     (lambda (a-2d-object)
       (--- 'synchronize)
       (let ((current-tick 0)(current-size start-size)(current-ang start-ang-val))
	 (while (< current-tick ticks)
	   (let ((current-object (--- 'get-new-object)))
	     (-- a-2d-object 'add-element current-object)
	     (-- current-object 'move-to-point (bell-random-point bounds-range))
	     (if (not (= size-iteration 0))
		 (progn
		   (-- current-object 'size current-size) 
		   (setq current-size (+ current-size size-iteration))))
	     (if (not (= ang-iteration 0))
		 (progn
		   (-- current-object 'spin current-ang)
		   (setq current-ang (+ current-ang ang-iteration)))))
	   
	   (message (concat (number-to-string current-tick) "/" (number-to-string ticks)))
	   (setq current-tick (+ current-tick 1))))))))



; utility functions

(defun bell-random-point (limit)
  (let ((new-point (! "2d-point"))(twice-limit (* 2 limit)))
    (-- new-point 
	'set-x 
	(- (truncate 
	    (/ 
	     (+ (random twice-limit) 
		(+ (random twice-limit) 
		   (random twice-limit))) 3)) limit))
    (-- new-point 'set-y 
	(- (truncate 
	    (/ 
	     (+ (random twice-limit) 
		(+ (random twice-limit) 
		   (random twice-limit))) 3)) limit))
    new-point))