;


(defvar classes 
      (make-hash-table :test 'equal))

(defun def-class
  (name-string parent-class-name instance-var-list class-var-list method-list)
  (let ((new-class (make-hash-table))(defs (make-hash-table)))
    (puthash name-string new-class classes)
    (puthash 'type "class" new-class)
    (puthash 'definitions defs new-class)

    (puthash 'parent parent-class-name defs)
    (puthash 'instance-vars instance-var-list defs)
    (puthash 'class-vars class-var-list defs)
    (puthash 'methods method-list defs)

    (puthash 'instance-vars (make-hash-table) new-class)
    (puthash 'class-vars (make-hash-table) new-class)
    (puthash 'methods (make-hash-table) new-class)


    (inherit new-class parent-class-name instance-var-list class-var-list method-list)
    (rebuild-classes-structure)
    new-class))


(defun inherit 
  (child-class parent-class-name instance-var-list class-var-list method-list)
    ;inheritance of superclass data and methods
    (if parent-class-name
	(let 
	    ((parent-instance-var-hash 
	      (gethash 'instance-vars (gethash parent-class-name classes)))
	     (parent-class-var-hash 
	      (gethash 'class-vars (gethash parent-class-name classes)))
	     )
	  (maphash (lambda (key value)
	     (puthash key value (gethash 'instance-vars child-class)))
	   parent-instance-var-hash)
	  (maphash (lambda (key value)
	     (puthash key value (gethash 'class-vars child-class)))
	   parent-class-var-hash)
	  ))
    ;add new instance-vars
    (while instance-var-list
      (let ((key (car (car instance-var-list)))(value (car (cdr (car instance-var-list)))))
	(puthash key value (gethash 'instance-vars child-class)))
      (setq instance-var-list (cdr instance-var-list)))
    ;add new class-vars
    (while class-var-list
      (let ((key (car (car class-var-list)))(value (car (cdr (car class-var-list)))))
	(puthash key value (gethash 'class-vars child-class)))
      (setq class-var-list (cdr class-var-list)))
    ;update class-path for class
    (puthash 'class-path 
	     (cons child-class 
	      (gethash 'class-path (gethash 'class-vars child-class)))
	     (gethash 'class-vars child-class))
    ;update time-stamp for class
    (puthash 'time-stamp (current-time) (gethash 'class-vars child-class))
    ;add new methods
    (while method-list
      (let ((key (car (car method-list)))(value (car (cdr (car method-list)))))
;binding functionality here -- if value is 'bind-method, search ancestry and overwrite value
	(if (eq value 'bind-method)
	    (let ((class-path (gethash 'class-path (gethash 'class-vars child-class))) 
		  current-class)
	      (while class-path
		(setq current-class (car class-path))
		(setq value (gethash key (gethash 'methods current-class))) 
		(setq class-path (cdr class-path)))))
	(puthash key value (gethash 'methods child-class))
	(setq method-list (cdr method-list)))))


(defun rebuild-classes-structure ()
  (let ((mod-list '()))
   ;add time invalidated classes to mod-list
  (maphash (lambda (key value)
	     (let 
		 ((class-variables (gethash 'class-vars value))
		  (class-time nil)
		  (class-path nil))
	       (progn
		 (setq class-time (gethash 'time-stamp class-variables))
		 (setq class-path (gethash 'class-path class-variables)) 
		   (while class-path
		     (progn
		       (if (time-less-p class-time 
					(gethash 'time-stamp (gethash 'class-vars (car class-path))))
			   (cons value mod-list))
		     (setq class-path (cdr class-path)))))))
	   classes)
  (if (> (length mod-list) 0)
      (progn
	(sort mod-list
	      (lambda (first second)
		(<=
		 (length (gethash 'class-path (gethash 'class-vars first)))
		 (length (gethash 'class-path (gethash 'class-vars second))))))
	(while mod-list
	  (let 
	      ((current-class (car mod-list))
	       (current-definitions  nil))
	    (progn
	      (setq current-definitions (gethash 'definitions current-class)) 
	      (inherit 
	       current-class 
	       (gethash 'parent current-definitions) 
	       (gethash 'instance-vars current-definitions)
	       (gethash 'class-vars current-definitions)
	       (gethash 'methods current-definitions))
	      (setq mod-list (cdr mod-list)))))))))


(defun instance (my-class-string &rest data)
  (let ((new-instance (make-hash-table))
	(instance-data (gethash 'instance-vars (gethash my-class-string classes))))
    (maphash
     (lambda (key value)
       (puthash key value new-instance))
     instance-data)
    (puthash 'class my-class-string new-instance)
    (while data
      (let ((key (car (car data)))(value (car (cdr (car data)))))
	(puthash key value new-instance)
      (setq data (cdr data))))
    (-- new-instance 'init) 
    new-instance))

(defvar selfstack '())

(defun get-self ()
  (car selfstack))


; only to be called inside object methods which need to update internal data 
; before method conclusion
(defun sos-set (quoted-sym value)
  (set quoted-sym value)
  (-- (get-self) 'set quoted-sym value))

(defun -- (object the-message &rest data)
;  (if selfstack
;      (message (concat "stackdepth: " (number-to-string (length selfstack))))
;    (message "stackdepth: 0"))


;why does the following not seem to work?
  (if selfstack
      (let ((self (get-self)))
	(maphash 
	 (lambda (key value)
	   (puthash key (eval key) self))
	 self)))

  (if object
      (let ((identical-to-caller nil)(temp-self nil))
	(setq temp-self (get-self))
  ;self stack push  
	(setq selfstack (cons object selfstack))
	(let 
	    ((my-class (gethash (gethash 'class object) classes)) 
	     (return-value nil)
	     (self (get-self))
	     current-method 
	     self-var-list)
	  (if (eq temp-self self) 
	      (progn
;		(message "identical caller")
		(setq identical-to-caller t))
	    (setq identical-to-caller nil))
	  (setq current-method (gethash the-message (gethash 'methods my-class)))
	  (setq self-var-list ())
	  (if (not identical-to-caller)
	      (maphash
	       (lambda (key value)
		 (set 'self-var-list (cons (list key `(quote ,value))  self-var-list)))
	       self))
					;    (print self-var-list)
	  (if (eq current-method nil) ; walk up tree
	      (let ((class-path (gethash 'class-path (gethash 'class-vars my-class)))
		    current-class)
		(while (and (eq current-method nil) class-path)
		  (setq current-class (car class-path))
		  (setq current-method (gethash the-message (gethash 'methods current-class))) 
		  (setq class-path (cdr class-path)))))
	  (if (eq current-method nil) ; if still nil after all that...
;       	(setq return-value (concat "sos error: no method called '" (symbol-name the-message) "'"))
	      (progn
		(setq selfstack (cdr selfstack)) ; cleanup self stack for error
		(debug (concat "sos error: no method called '" (symbol-name the-message) "' in " (gethash 'class object))))
      ; as the parameter list for let below is bound to a variable, the backquote hack was necessary
      ; because let is a special form that controls evaluation within the parameter list
	    (eval `(let ,self-var-list
		     (if data
			 (setq return-value 
			       (apply current-method data))
		       (setq return-value
			     (funcall current-method)))
		     (maphash
		      (lambda (key value)
			(puthash key (eval key) self))
		      self)
		     )))
	  (setq selfstack (cdr selfstack))
	  return-value))
    (debug (concat "sos error: --, Object is nil, Message: " (symbol-name the-message)))))

;system base class: object
(def-class 
 ;name string
  "object" 
 ;parent class
  nil 
 ;instance-vars
  nil 
 ;class-vars
  '((class-path ())) 
 ;methods
  '(
    (init
     (lambda ()))
    (set 
     (lambda 
       (key value)
       (set key value)))
    (set* 
     (lambda 
       (data)
       (while data
	 (let ((key (car (car data)))(value (car (cdr (car data)))))
	 (set key value)
	 (setq data (cdr data))))))    
     (get
     (lambda 
       (key)
       (eval key)))
    (get-class-var
     (lambda 
       (key)
       (gethash key 
		(gethash 'class-vars
			 (gethash (-- self 'class-name ) classes)))))
    (set-class-var
     (lambda 
       (key value)
       (puthash key value 
		(gethash 'class-vars
			 (gethash (-- self 'class-name ) classes)))))    
    (class-name
     (lambda 
       ()
       (gethash 'class self)))
    (is-a?
     (lambda (class-string)
       (let ((test-class (gethash class-string classes))
	     (class-path (-- self 'get-class-var 'class-path)))
	 (if (memq test-class class-path)
	     t
	   nil))))
    (object-instance?
     (lambda (other)
       (if (hash-table-p other)
	   (if (gethash 'class other)
	       t
	     nil)
	 nil)))
    (equiv?
     (lambda (other)
       (if (-- self 'object-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	    (maphash
		(lambda (key value)
		  (if (-- self 'object-instance? value)
		      (setq obj-check (-- value 'equiv? (-- other 'get key))))
		  (if (not (or (equal (-- other 'get key) value) obj-check))
		      (setq y-or-n nil)))
		self)
	     y-or-n)
	 nil)))
    (equal?
     (lambda (other)
       (if (-- self 'object-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	    (maphash
		(lambda (key value)
		  (if (-- self 'object-instance? value)
		      (setq obj-check (-- value 'equiv? (-- other 'get key))))
		  (if (not (or (equal (-- other 'get key) value) obj-check))
		      (setq y-or-n nil)))
		self)
	     y-or-n)
	 nil)))

))

