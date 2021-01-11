;standard library classes... rely on sos.el


; state class - abstract class: descendents treat all instance data as stacks (lists)

(sos-class 
 "state" "object"
 nil
 '((init after-ancestor
    (lambda ()
      (let ((var-list (--- 'get-class-var 'parameter-order))(current-var nil)(current-value nil))
	(while var-list
	  (setq current-var (car var-list))
	  (setq current-value (eval current-var))
	  (if (not (listp current-value))
	      (set current-var (list current-value)))
	  (setq var-list (cdr var-list))))))
   (get 
    (lambda (a-symbol)
      (if (member a-symbol (--- 'get-class-var 'parameter-order))
	  (car (eval a-symbol))
	(message
	 (concat
	  "ERROR: state.get: "
	  (symbol-name a-symbol)
	  " symbol not defined")))))
   (set 
    (lambda (a-symbol a-value)
      (if (member a-symbol (--- 'get-class-var 'parameter-order))
	  (progn
	    (set a-symbol (cons a-value (eval a-symbol)))
	    (car (eval a-symbol)))
	(message
	 (concat
	  "ERROR: state.set: "
	  (symbol-name a-symbol)
	  " symbol not defined")))))

   (pop 
    (lambda (a-symbol)
      (if (member a-symbol (--- 'get-class-var 'parameter-order))
	  (if (cdr (eval a-symbol))
	      (let ((current-value (car (eval a-symbol))))
		(set a-symbol (cdr (eval a-symbol)))
		current-value)
	    (message
	     (concat
	      "ERROR: state.pop: "
	      (symbol-name a-symbol)
	      " cannot pop base element")))
	(message
	 (concat
	 "ERROR: state.pop: "
	 (symbol-name a-symbol)
	 " symbol not defined")))))
   (get-list 
    (lambda (a-symbol)
      (if (member a-symbol (--- 'get-class-var 'parameter-order))
	  (eval a-symbol)
	(message
	 "ERROR: state.get-list: symbol not defined"))))))







; classes dealing with files
 
(sos-class 
 "buffer-file" "object"
 '((filename nil)(buffer nil)(pathstring "/Users/gregorkoomey/dev/emacs/current/eps/"))
  '(
    (init
     (lambda ()
       (if filename
	   (puthash 'buffer
		    (find-file (concat 
				pathstring
				filename
				))
		    self)
	 (message "Error: buffer file instance: no filename"))))
    (save
     (lambda ()
       (save-excursion
	 (set-buffer filename)
	 (save-buffer))))
    (clear
     (lambda ()
       (save-excursion
	 (set-buffer filename)
	 (erase-buffer))))
    (write-string 
     (lambda (string)
       (save-excursion
	 (set-buffer filename)
	 (goto-char (point-max))
	 (insert string))))
    (write-line
     (lambda (list-of-strings)
       (save-excursion
	 (set-buffer filename)
	 (goto-char (point-max))
	 (while list-of-strings
	   (insert (car list-of-strings) " ")
	   (setq list-of-strings (cdr list-of-strings)))
	 (newline))))
    (string-value
     (lambda ()
       (save-excursion
	 (set-buffer filename)
	 (buffer-string))))
    (new-line
     (lambda ()
       (save-excursion
	 (set-buffer filename)
	 (goto-char (point-max))
	 (newline))))))


(sos-class "input-file" 
  "object" 
  '(
    (filename nil)
    (buffer nil)
    (pathstring "/Users/gregorkoomey/dev/emacs/current/eps/")
    (string-value nil))  
  '((init
     (lambda ()
       (if filename
	   (progn
	     (set 'buffer
		      (find-file-noselect 
		       (concat 
			pathstring
			filename)))
	     (set 'string-value
		     (save-excursion
			 (set-buffer filename)
			 (buffer-string))))
	 (message "Error: input file instance: no filename"))))
    (string-value
     (lambda ()
       string-value))))

(sos-class "output-file"
  "buffer-file"
  '((prolog-file nil)
    (trailer-file nil))
  '((init
     (lambda ()
       (if filename
	   (progn
	     (puthash 'buffer
		      (find-file 
		       (concat 
			pathstring
			filename))
		      self)
	     (save-excursion
	       (set-buffer filename)
	       (erase-buffer)
	       (if prolog-file
		   (progn
		     (--- 'write-string 
			 (-- prolog-file 'string-value))
		     (--- 'new-line)
		     (goto-char (point-max))))))  
	 (message "Error: output file instance: no filename"))))
    (save
     (lambda ()
       (save-excursion
	 (set-buffer filename)
	 (goto-char (point-max))
	 (newline)
	 (if trailer-file
	     (--- 'write-string 
		 (-- 
		  trailer-file 
		  'string-value)))	 
	 (save-buffer))))))



