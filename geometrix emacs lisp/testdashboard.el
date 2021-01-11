;reload geometrix systems
(progn
  (load "library")
  (load "sos-2")
  (load "standard")
  (load "geo-metric")
  (load "2d-space")
  (load "geometry")
  (load "eps")
  (load "2d-objects")
  (load "spigot")
  (load "path-factory")
  (load "perspective")
  (load "3d-space")
)

(geo-reset-output-file)

(geo-save-output-file)
 




;create general test output file
(setq test-out (! "output-file" '(filename "testoutput.txt")))

;clear output file
(-- test-out 'clear)


; create eps output 
(setq test-prolog (instance "input-file" '(filename "prolog.eps")))
(setq test-trailer (instance "input-file" '(filename "trailer.eps")))
(setq test-doc 
      (instance "output-file" 
		'(filename "yowza.ai")
		`(prolog-file ,test-prolog)
		`(trailer-file ,test-trailer )))  





;reset classes table
(progn
  (setq classes nil)
  (setq classes 
      (make-hash-table :test 'equal)))


;reset selfstack
(setq selfstack ())

