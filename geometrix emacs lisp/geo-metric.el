; relies on sos

(sos-class 
 "geo-metric" "object"
 '((graphics-state class))
 nil)


(sos-class 
 "graphics-state" "state"
 '((stroke-width (1.0))
   (stroke-color (0.0))
   (fill-color (0.0)))
 '((write-graphics-state
    (lambda ()
      (-- geo-output-file 'write-string (concat (number-to-string (car stroke-width)) " w\n"  ))
      (-- geo-output-file 'write-string (concat (number-to-string (car stroke-color)) " G\n"  ))
      (-- geo-output-file 'write-string (concat (number-to-string (car fill-color)) " g\n"  ))))))

(sos-set-class-var "geo-metric" 'graphics-state (! "graphics-state"))



(defvar geo-output-file)

(defun geo-reset-output-file ()
  (setq geo-output-file
	(! "output-file" 
	   '(filename "output.ai") 
	   `(prolog-file ,(! "input-file" '(filename "prolog.eps")))
	   `(trailer-file ,(! "input-file" '(filename "trailer.eps")))))
  (-- (sos-get-class-var "geo-metric" 'graphics-state) 'write-graphics-state))


(defun geo-save-output-file ()
  (-- (sos-get-class-var "geo-metric" 'graphics-state) 'write-graphics-state)
  (-- geo-output-file 'save))


