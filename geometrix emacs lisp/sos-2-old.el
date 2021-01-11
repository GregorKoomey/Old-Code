; second stab at simple object system

; 101305


; system consists of:

; Global data

(defvar self nil)
(defvar sos-classes nil)
(setq sos-classes (make-hash-table :test 'equal))

; Global functions

; sos-class :: defines class in sos-classes
; (sos-class "class-name" "parent-class-name" '(variables) '(methods))

; ! :: creates instance of class
; (! "class-name" &rest data)

; -- (while in dev, ---) :: pass message to object
; (-- obj 'message data)

; --- (while in dev, ----) :: fire method 


; NOTES
; - simplicity of implementation of class structure requires that the entire system be rebuilt 
;   when anything that has been subclassed is changed (fortunately this does not appear to be an issue
;   - ADV: a system can be put in place to rebuild selective parts of a given tree, depending on timestamp.

; - data can be initialized through "!" calls by specifying a number of variables which are mapped to instance vars in order
; this allows us to abandon the "create function to build instances" practice



(defun sos-class (class-name parent-name vars methods)
  (let ((new-class (make-hash-table))
	(parent-class nil)
	parameter-order)
    (if parent-name 
	(setq parent-class (gethash parent-name sos-classes)))
    (if (and (not parent-class) (not (equal class-name "object"))) ; *!* note hardcoded check for "object" as only valid system root
	(message (concat 
		  "\nWARNING:\nsos-class: " 
		  class-name
		  " definition\nparent-class: "
		  parent-name
		  " is not in system\n\n")))
    ; add to system
    (puthash class-name new-class sos-classes)
    (puthash 'type "class" new-class)
    ; inherit structure from parent (unless parent is nil, then build structure)
    (if parent-class
	(progn 
	  (puthash 'instance-vars (copy-hash-table (gethash 'instance-vars parent-class)) new-class)
	  (puthash 'class-vars  (copy-hash-table (gethash 'class-vars parent-class)) new-class)
	  (puthash 'shadow-vars  (copy-hash-table (gethash 'shadow-vars parent-class)) new-class)
	  (puthash 'methods  (copy-hash-table (gethash 'methods parent-class)) new-class)
	  (setq parameter-order (gethash 'parameter-order (gethash 'class-vars parent-class))))
      (progn
	(puthash 'instance-vars (make-hash-table) new-class)
	(puthash 'class-vars (make-hash-table) new-class)
	(puthash 'class-path nil (gethash 'class-vars new-class))
	(puthash 'shadow-vars (make-hash-table) new-class)
	(puthash 'methods (make-hash-table) new-class)))
    ; update class-path : note this implementation uses class-name rather than the class itself.
    (puthash 'class-path (cons class-name (gethash 'class-path (gethash 'class-vars new-class))) (gethash 'class-vars new-class))
    ; update new data
    (while vars
      (let ((current-item nil)
	    (current-symbol nil)
	    (current-mode nil)
	    (current-value nil)
	    (current-temp nil)
	    (instance (gethash 'instance-vars new-class))
	    (class (gethash 'class-vars new-class))
	    (shadow (gethash 'shadow-vars new-class)))
	; process current variable item
	(setq current-item (car vars))
	(if (not (listp current-item))
	    (progn
	      (setq current-symbol current-item)
	      (setq current-mode nil)
	      (setq current-value nil))
	  (progn
	    (setq current-symbol (car current-item))
	    (setq current-item (cdr current-item))
	    (while current-item
	      (setq current-temp (car current-item))
	      (cond
	       ((and (symbolp current-temp) (member current-temp '(shadow class)))
		(setq current-mode current-temp))
	       (t 
		(setq current-value current-temp)))
	  (setq current-item (cdr current-item)))))
	; put it where it's going
	(if current-mode
	    (if (member current-mode '(instance class shadow))
		(puthash current-symbol current-value (eval current-mode))
	      (message (concat 
			"\nERROR:\nsos-class: " 
			class-name 
			" "
			(symbol-name current-symbol)
			" definition\n  variable mode symbol: "
			(symbol-name current-mode)
			" not recognized;\n  variable ignored...\n\n")))
	  (progn
	    (setq parameter-order (append parameter-order (list current-symbol)))
	    (puthash current-symbol current-value instance))))
      (setq vars (cdr vars)))
    (puthash 'parameter-order parameter-order (gethash 'class-vars new-class))

;    (puthash 'class class-name (gethash 'instance-vars new-class))
    ; update new methods
    (while methods
      (let ((current-item nil)
	    (current-symbol nil)
	    (current-mode nil)
	    (current-temp nil)
	    (ancestor-method nil)
	    (current-lambda nil)
	    (current-docstring nil) ; unused... to be implemented
	    (method-hash (gethash 'methods new-class)))
;	(message "method handling start")
	(setq current-item (car methods))
	(setq current-symbol (car current-item))
	(setq current-item (cdr current-item))
	(while current-item
	  (setq current-temp (car current-item))
	  (cond
	   ((functionp current-temp)
	    (setq current-lambda current-temp))
	   ((symbolp current-temp)
	    (setq current-mode current-temp))
	   ((stringp current-temp)
	    (setq current-docstring current-temp)))
	  (setq current-item (cdr current-item)))

;	(message "method handling after data collection")

	(if (and parent-class current-mode)
	    (setq ancestor-method (gethash current-symbol (gethash 'methods parent-class))))
;	(message "method handling after parent-class/current-mode if")
	(if (and (not ancestor-method) current-mode)
	    (progn
	      (message (concat 
			"\nERROR:\nsos-class: " 
			class-name 
			" "
			(symbol-name current-symbol)
			" definition\n"
			(symbol-name current-mode) " flag invalid -- no corresponding parent method;\n  default to override...\n\n"))
	      (setq current-mode nil)))

;	(message "method handling after parent-class check")

	(if (symbolp current-mode)
	    (if (member current-mode '(before-ancestor after-ancestor))
		(progn
;		  (message "inside current-mode check")
;		  (setq current-lambda (car (cdr (cdr current-item)))) ; error check?
;		  (message "after set current-lambda")
		  (cond
		   ((equal current-mode 'before-ancestor)
;		    (message "start before ancestor processing")
		    (if (functionp ancestor-method) 
			(setq current-lambda 
			      (cons current-lambda (list ancestor-method)))
		      (setq current-lambda (cons current-lambda ancestor-method)))
;		    (message "end before ancestor processing")
		    )
		   ((equal current-mode 'after-ancestor)
;		    (message "start after ancestor processing")
		    (if (functionp ancestor-method) 
			(setq current-lambda  
			      (cons ancestor-method (list current-lambda)))
		      (setq current-lambda (append ancestor-method (list current-lambda))))
;		    (message "end after ancestor processing")
		    )
		   (t ))))
	      (message (concat 
			"\nERROR:\nsos-class: " 
			class-name 
			" "
			current-symbol
			" definition\n  method mode symbol not recognized;\n  default to override...\n\n")))
;	(print `(,current-symbol ,current-lambda ,current-mode))
	(puthash current-symbol current-lambda method-hash)
      (setq methods (cdr methods))))
    new-class))

; check if class
(defun sos-class? (a-possible-class)
  (if (hash-table-p a-possible-class)
   (and
    (gethash 'instance-vars a-possible-class)
    (gethash 'class-vars a-possible-class)
    (gethash 'shadow-vars a-possible-class)
    (gethash 'methods a-possible-class)
    a-possible-class)
   nil))
    


; this is a debug function, to see what sos-class is actually building
; it is intended to eventually be used to build real sos debug functionality
(defun sos-class-to-list (a-class)
  (let ((data-list nil))
    (setq data-list (cons (cons 'methods (hash-to-let-list (gethash 'methods a-class))) data-list))
    (setq data-list (cons (cons 'shadow-vars (hash-to-let-list (gethash 'shadow-vars a-class))) data-list))
    (setq data-list (cons (cons 'class-vars (hash-to-let-list (gethash 'class-vars a-class))) data-list))
    (setq data-list (cons (cons 'instance-vars (hash-to-let-list (gethash 'instance-vars a-class))) data-list))
    data-list))

(defun ! (class-name &rest data)
  (if (gethash class-name sos-classes)
      (let ((new-instance nil)
	    (parent-class (gethash class-name sos-classes)))
	(setq new-instance (copy-hash-table (gethash 'instance-vars parent-class)))
	(puthash 'class class-name new-instance)
	(puthash 'shadow-vars (gethash 'shadow-vars parent-class) new-instance) ; *!* might need to copy shadow-vars instead of just referencing
	(if (gethash 'init (gethash 'methods parent-class)) 
	    (--- new-instance 'init))
	(if data
	    (let ((variables (gethash 'parameter-order (gethash 'class-vars parent-class)))
		  (ordered-vars nil)
		  (ordered t)
		  (current-element nil)
		  (current-value nil)
		  (current-key nil)
		  (current-temp nil))
	      (setq ordered-vars variables)
	      (while data
		(setq current-temp (car data))
		(if (and (listp current-temp) current-temp)
		    (if (and (= (length current-temp) 2) (member (car current-temp) variables))
			(progn
			  (setq current-key (car current-temp))
			  (setq current-value (car (cdr current-temp)))
			  (puthash current-key current-value new-instance)
			  (setq ordered nil))
		      (if (and ordered ordered-vars)
			  (progn
			    (setq current-key (car ordered-vars))
			    (setq current-value current-temp)
			    (puthash current-key current-value new-instance)
			    (setq ordered-vars (cdr ordered-vars)))
			(message (concat 
				  "\nERROR:\n!: " 
				  class-name 
				  " bad variable order;\n\n"))))
		  (progn
		    (if ordered-vars
			(progn
			  (setq current-key (car ordered-vars))
			  (if current-temp
			      (progn
				(setq current-value current-temp)
				(puthash current-key current-value new-instance)))
			  (setq ordered-vars (cdr ordered-vars)))
		      (message (concat 
				"\nERROR:\n!: " 
				class-name 
				" too many variables specified;\n\n")))))
		(setq data (cdr data)))))

	new-instance)
  (message (concat 
	    "\nERROR:\n!: " 
	    class-name 
	    " class name not recognized;\n\n"))))
 
; check if class - if true, returns instance, else nil
(defun sos-instance? (a-possible-instance)
  (if (hash-table-p a-possible-instance)
   (and
    (gethash 'class a-possible-instance)
    (gethash 'shadow-vars a-possible-instance)
    a-possible-instance)
   nil))




(defvar sos-environment-list) ; outside function for system efficiency (?) (how expensive is "let"?)
(defvar sos-current-class) ; ditto...

(defun --- (current-object message-symbol &rest data)
  (if self (sos-store-data))
  (setq sos-current-class (gethash (gethash 'class current-object) sos-classes))
  (if (not (eq self current-object))
      (setq sos-environment-list
	    (append
	      `((self ,current-object))
	      `((methods ,(gethash 'methods sos-current-class)))
	      (hash-to-let-list current-object 'shadow-vars)
	      (hash-to-let-list (gethash 'shadow-vars current-object))
	      `((shadow-vars ,(gethash 'shadow-vars sos-current-class)))
	      `((class-vars ,(gethash 'class-vars sos-current-class)))))
    (setq sos-environment-list `()))
;    (setq sos-environment-list `((self ,current-object))))
  (eval 
   `(let ,sos-environment-list
      ,(if data
	   `(---- message-symbol (car data))
	 `(---- message-symbol))

      (sos-store-data)
      (sos-reset-shadow)))
;	     (setq sos-environment-list nil)  ;leave commented out for debugging
;	     (setq sos-current-class nil)
)

(defvar shadow-vars) ; to stop compiler whining

(defun sos-reset-shadow ()
;  (message "sos-reset-shadow")
;  (maphash
;   (lambda (key value)
;     (if value 
;     )
;   shadow-vars))
)


(defun sos-store-data ()
  (message "sos-store-data")
  (maphash
   (lambda (key value)
     (cond
      ((numberp value)
       (if (not (= value (eval key)))
	   (puthash key (eval key) self)))
      ((stringp value)
       (if (not (string-equal value (eval key)))
	   (puthash key (eval key) self)))
      ((or (sos-instance? value) (hash-table-p value))
       (if (not (eq value (eval key)))
	 (puthash key (eval key) self)))  
      (t
       (puthash key (eval key) self))))
   self))

(defvar methods) ; to stop compiler whining

; note: boundp functionality below is hack to stop compiler whining... keep an eye on it...
(defun ---- (message-symbol &rest data)
  (if self
      (let ((current-method (gethash message-symbol methods)))
;	(message "start ---- let")
	(if current-method
	    (progn
;	      	(message "current method not nil")
		(if (functionp current-method)
		    (if data
			(apply current-method data)
		      (funcall current-method))
		    (while current-method
;		      (print `(,current-method))
		      (if data    
			  (apply (car current-method) data)
			(funcall (car current-method)))
		      (setq current-method (cdr current-method)))))
	  (message (concat 
		    "\nERROR:\n----: " 
		    message-symbol 
		    " :  message not recognized in class: " 
		    (if (boundp 'class) class)
		    "\n\n"))))
    (message (concat 
	      "ERROR:\n----: " 
	      message-symbol 
	      " :  self is nil... " 
	       (if (boundp 'class) class)
	       "\n\n"))))
    



;system base class: object
(sos-class 
 ;name string
  "object" 
 ;parent class
  nil 
 ;vars
  nil
 ;methods
  '(
    (init ; abstract method
     (lambda ()))
    (re-init
     (lambda ()))
    (set 
     (lambda 
       (key value)
       (set key value)))
    (set* ; this should be updated to the default instance functionality
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
       (gethash key class-vars)))
     (set-class-var
     (lambda 
       (key value)
       (puthash key value class-vars)))
    (class-name
     (lambda 
       ()
       class))
    (is-a? ; this is just fucking wrong
     (lambda (class-string)
       (if (memq test-class class)
	     t
	   nil)))
    (equiv?
     (lambda (other)
       (if (---- 'object-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	    (maphash
		(lambda (key value)
		  (if (---- 'object-instance? value)
		      (setq obj-check (--- value 'equiv? (--- other 'get key))))
		  (if (not (or (equal (--- other 'get key) value) obj-check))
		      (setq y-or-n nil)))
		self)
	     y-or-n)
	 nil)))
    (equal?
     (lambda (other)
       (if (---- 'object-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	    (maphash
		(lambda (key value)
		  (if (---- 'object-instance? value)
		      (setq obj-check (-- value 'equiv? (-- other 'get key))))
		  (if (not (or (equal (-- other 'get key) value) obj-check))
		      (setq y-or-n nil)))
		self)
	     y-or-n)
	 nil)))

))


