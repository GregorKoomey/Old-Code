; second stab at simple object system

; 101305


; system consists of:

; Global data

(defvar self nil)
(defvar sos-classes nil)
(setq sos-classes (make-hash-table :test 'equal))

; Global functions (presented in order of importance)

; sos-class 
;  defines class in sos-classes
;   (sos-class "class-name" "parent-name" '(variables) '(methods))

; ! 
;  creates instance of class
;   (! "class-name" &rest data)

; --  
;  pass message to object
;   (-- obj 'message data)

; ---  
;  fire local method 
;   (--- 'message data)
;    "---" is equivalent to "(-- self 'message data)"

; "object" 
;   Root class definition for sos that includes standard functionality such as "init", "set", "get", "set-class-var", etc.
;    - should be only class with parent of nil 

; sos-class?
;   Checks data reference to see if it's an sos class.


; sos-instance?
;   Checks data reference to see if it's an instance of an sos class





; sos-class function details
;  Builds class hash table from description data, stores in sos-classes (global) and returns new-class

;  example call:
;   (sos-class "class-name" "parent-name" '(variables) '(methods))

;     "class-name" is a string that serves as key for the sos-classes global hashtable
;        subsequent references to the class are invoked through (gethash "class-name" sos-classes)


;     "parent-name" is a string that refers to the parent class

;     '(variables) denotes a quoted list of instance and class variables
;       conventions:
;         List can use standard let-list notation, where an element consists of either a list or a single symbol.
;           - '((symbol-1 value-1) symbol-2 (symbol-3 value-3))
;             - symbol-1 is bound to value-1, symbol-2 is bound to nil and symbol-3 is bound to value-3
;             - first (or only) item in element must be a symbol.

;         Optional "class" keyword can be added to a particular variable sublist to denote a class variable
;           - '((symbol-1 value-1) (symbol-2 class value-2))
;             - symbol-1 is bound to value-1 for object instance while symbol-2 is bound to class variable that defaults to value-2
;             - "class" keyword is unquoted (as the list itself should be quoted) and can be in any position but the first.


;     '(methods) denotes a quoted list of method definition lists
;       conventions:
;         List elements generally are lists of symbol/lambda list pairs 
;           - '((init (lambda () ...initialize data structures...))(something-else (lambda ()...)))

;         Optional keywords include "before-ancestor" and "after-ancestor", to manage override functionality
;           - '((init before-ancestor (lambda () ...))(something-else after-ancestor (lambda () ...)))
;             - "before-ancestor" runs the current method before running the parent class method of the same name.
;             - "after-ancestor" runs current method after running the parent class method of the same name 
;             - optional keywords can be in any position but the first (which must be a symbol)
;             - cascading functions can be implemented by simply passing it on. 
;             - "init" is generally treated as an after-ancestor method.

; ERRORS:
;   - class not recognized
;   - parent class not recognized
;   - optional variable flag symbol that is unrecognized
;   - optional method flag symbol that is unrecognized 
;   - parent method override option is used (before/after-ancestor) and corresponding parent method exists.
; 



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
	  (puthash 'methods  (copy-hash-table (gethash 'methods parent-class)) new-class)
	  (setq parameter-order (gethash 'parameter-order (gethash 'class-vars parent-class))))
      (progn
	(puthash 'instance-vars (make-hash-table) new-class)
	(puthash 'class-vars (make-hash-table) new-class)
	(puthash 'class-path nil (gethash 'class-vars new-class))
	(puthash 'methods (make-hash-table) new-class)))
    ; update class-path : note this implementation uses a symbol made from class-name rather than the class itself.
    (puthash 'class-path (cons (intern class-name) (gethash 'class-path (gethash 'class-vars new-class))) (gethash 'class-vars new-class))
    ; update new data
    (while vars
      (let ((current-item nil)
	    (current-symbol nil)
	    (current-mode nil)
	    (current-value nil)
	    (current-temp nil)
	    (instance (gethash 'instance-vars new-class))
	    (class (gethash 'class-vars new-class)))
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
	       ((and (symbolp current-temp) (member current-temp '(class)))
		(setq current-mode current-temp))
	       (t 
		(setq current-value current-temp)))
	  (setq current-item (cdr current-item)))))
	; put it where it's going
	(if current-mode
	    (if (member current-mode '(instance class))
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
	(if (and parent-class current-mode)
	    (setq ancestor-method (gethash current-symbol (gethash 'methods parent-class))))
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
	(if (symbolp current-mode)
	    (if (member current-mode '(before-ancestor after-ancestor))
		(progn
		  (cond
		   ((equal current-mode 'before-ancestor)
		    (if (functionp ancestor-method) 
			(setq current-lambda 
			      (cons current-lambda (list ancestor-method)))
		      (setq current-lambda (cons current-lambda ancestor-method))))
		   ((equal current-mode 'after-ancestor)
		    (if (functionp ancestor-method) 
			(setq current-lambda  
			      (cons ancestor-method (list current-lambda)))
		      (setq current-lambda (append ancestor-method (list current-lambda)))))
		   (t ))))
	      (message (concat 
			"\nERROR:\nsos-class: " 
			class-name 
			" "
			current-symbol
			" definition\n  method mode symbol not recognized;\n  default to override...\n\n")))
	(puthash current-symbol current-lambda method-hash)
      (setq methods (cdr methods))))
    new-class))

; sos-class implementation details
;   - ancestor override methods are stored as lists of lambda lists. "fire" functionality handles either list of lambdas or just lambda



; check if class
;  - returns class or nil

; usage: 
;  (sos-class? a-possible-class)

(defun sos-class? (a-possible-class)
  (if (hash-table-p a-possible-class)
   (and
    (gethash 'instance-vars a-possible-class)
    (gethash 'class-vars a-possible-class)
    (gethash 'methods a-possible-class)
    a-possible-class)
   nil))
    


(defun sos-set-class-var (class-name class-var-name class-var-value)
  (let ((current-class (gethash class-name sos-classes)))
    (if (sos-class? current-class)
	(puthash class-var-name class-var-value (gethash 'class-vars current-class))
      (message (concat 
		"\nERROR:\nsos-set-class-var: " 
		class-name 
		"clas-name not recognized")))))

(defun sos-get-class-var (class-name class-var-name)
  (let ((current-class (gethash class-name sos-classes)))
    (if (sos-class? current-class)
	(gethash class-var-name (gethash 'class-vars current-class))
      (message (concat 
		"\nERROR:\nsos-get-class-var: " 
		class-name 
		"clas-name not recognized")))))


; this is a debug function, to see what sos-class is actually building
; it is intended to eventually be used to build real sos debug functionality
(defun sos-class-to-list (a-class)
  (let ((data-list nil))
    (setq data-list (cons (cons 'methods (hash-to-let-list (gethash 'methods a-class))) data-list))
    (setq data-list (cons (cons 'class-vars (hash-to-let-list (gethash 'class-vars a-class))) data-list))
    (setq data-list (cons (cons 'instance-vars (hash-to-let-list (gethash 'instance-vars a-class))) data-list))
    data-list))



; ! function details
;   creates instance of class (previously defined in sos-classes) and assigns any appropriate initialization data.

;  equivalent example calls (assumes point object which has x, y and z variables): 
;    (! "point" 2 3 5) ; preferred syntax
;    (! "point" '(x 2) '(y 3) '(z 5))
;    (! "point 2 3 '(z 5))

;    parameter-list class-variable is an ordered list of all instance variable symbols (that defines the above preferred syntax).
;    parameter-list is set automatically at class creation, but can be changed to affect the expected order.

; ERRORS:
;  - class name not recognized
;  - bad variable order, when non-list pair variable is specified after list-pair... invalid syntax
;  - too many variables, when more values are specified than there are symbols in the parameter-list

(defun ! (class-name &rest data)
  (if (gethash class-name sos-classes)
      (let ((new-instance nil)
	    (parent-class (gethash class-name sos-classes)))
	(setq new-instance (copy-hash-table (gethash 'instance-vars parent-class)))
	(puthash 'class class-name new-instance)
	(sos-set-class-var class-name 'instance-count (+ (sos-get-class-var class-name 'instance-count) 1))

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
	(if (gethash 'init (gethash 'methods parent-class)) 
	    (-- new-instance 'init))
	new-instance)
  (message (concat 
	    "\nERROR:\n!: " 
	    class-name 
	    " class name not recognized;\n\n"))))
 
; check if class - if true, returns instance, else nil

; usage: 
;  (sos-instance? a-possible-instance)

(defun sos-instance? (a-possible-instance)
  (if (hash-table-p a-possible-instance)
   (and
    (gethash 'class a-possible-instance)
    a-possible-instance)
   nil))



; -- function details
;   Sends message to object instance.
;     Sets "self" and extracts lexical environment info from object and class, then fires method.
;     if previous self, saves previous before setting up new one, then saves current self after firing method (--- 'method).

;  usage:
; (-- an-object-instance 'message) or (-- an-object-instance 'message ...any-data...)

;  an-object-instance is an instance of an sos object

; 'message is a symbol that corresponds to a lambda function

; any-data is defined by specific lambda bound to message symbol in class


; ERRORS:
;   all errors handled by call to "---" (fire)

;(defvar sos-environment-list) ; outside function for system efficiency (?) (how expensive is "let"?)
;(defvar sos-current-class) ; ditto...

(defun -- (current-object message-symbol &rest data)
  (if self (sos-store-data))
  (let (sos-current-class sos-environment-list return-value)
    (setq sos-current-class (gethash (gethash 'class current-object) sos-classes))
;    (print `("--: " ,data))
    (if (not (eq self current-object))
	(setq sos-environment-list
	      (append
	       `((self ,current-object))
	       (hash-to-let-list current-object)
	       `((methods ,(gethash 'methods sos-current-class)))
	       `((class-vars ,(gethash 'class-vars sos-current-class)))))
      (setq sos-environment-list `()))
    (eval 
     `(let ,sos-environment-list
	(setq return-value
	      ,(if data
		   `(---- message-symbol data)
		 `(---- message-symbol)))
	(sos-store-data)))
    return-value)
)



; sos-store-data function details
;   Resets state of self according to current lexical space.

;  usage:
; (sos-store-data)

; assumes self and corresponding lexical environment

; checks for equality of data and ignores, if so


(defun sos-store-data ()
;  (message "sos-store-data")
  (maphash
   (lambda (key value)
     (if (not (eval key))
	 (if value
	     (puthash key nil self))
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
)
     self))


; --- function details
;   Fires method in current lexical environment. Assumes self and methods hash table accessible in local namespace.

;  usage:
; (--- 'message) or (--- 'message ...any-data...)

; 'message is a symbol that corresponds to a lambda function

; any-data is defined by specific lambda bound to message symbol in class



; ERRORS:
;   - self is nil
;   - message not recognized in class



(defun hash-to-let-list (a-hash-table &rest exceptions)
;  (message "hash-to-let-list start")
  (let (new-list return-value)
      (if exceptions
	  (maphash 
	   (function 
	    (lambda (key value)
;	      (print `("hash-to-let-list w/excep: " ,key ,value))
	      (if (not (member key exceptions))
		  (setq new-list (cons (list key `(quote ,value)) new-list)))))
	   a-hash-table)
	(maphash 
	 (function 
	  (lambda (key value)
;	    (print `("hash-to-let-list no excep: " ,key ,value))
	    (setq new-list (cons (list key `(quote ,value)) new-list))))
	 a-hash-table))
    (setq return-value (eval `(quote ,new-list)))
;    (message "hash-to-let-list: right before end")
    return-value


))

(defun --- (message-symbol &rest data)
;  (print `("---: " ,message-symbol ,data))
  (if data ; handle return value? probably not necessary, as the body is so damned simple...
      (---- message-symbol data)
    (---- message-symbol)))

(defvar methods) ; to stop compiler whining
(defvar class)

(defun ---- (message-symbol &optional data)
  (if self
      (let ((current-method (gethash message-symbol methods))
;	    (data (car data))
	    (return-value nil))
;	(if (not (listp data))
;	    (setq data (list data)))
;	(print `("----: " ,message-symbol ,data))
	(if current-method
	    (progn
		(if (functionp current-method)
		    (progn
		      (setq return-value 
			    (if data
				(apply current-method data)
			      (funcall current-method))))
		  (while current-method
		    (setq return-value 
			  (if data    
			      (apply (car current-method) data)
			    (funcall (car current-method))))
		    (setq current-method (cdr current-method)))))
	  (message (concat 
		    "\nERROR:\n---: " 
		    (symbol-name message-symbol) 
		    " :  message not recognized in class: " 
		    class
		    "\n\n")))
;	(print `("--- return-value: " ,return-value))
	return-value)
    (message (concat 
	      "ERROR:\n---: " 
	      (symbol-name message-symbol) 
	      " :  self is nil... " 
	       class
	       "\n\n"))))
    
; "object" definition details
;   Base class object for sos system

;  usage:
;    subclass for sos class: root of object hierarchy


;system base class: object
(sos-class 
 ;name string
  "object" 
 ;parent class
  nil 
 ;vars
  '((dynamic-message class)
    (instance-count 0 class))
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
    (set* ; error check???
     (lambda (&rest data)
       (if data
	   (let ((variables (gethash 'parameter-order class-vars))
		 (ordered-vars nil)
		 (ordered t)
		 (current-element nil)
		 (current-value nil)
		 (current-key nil)
		 (current-temp nil)
		 (return-value nil))
	     (setq ordered-vars variables)
	     (while data
	       (setq current-temp (car data))
	       (if (and (listp current-temp) current-temp)
		   (if (and (= (length current-temp) 2) (member (car current-temp) variables))
		       (progn
			 (setq current-key (car current-temp))
			 (setq current-value (car (cdr current-temp)))
			 (set current-key current-value)
			 (setq return-value current-value)
			 (setq ordered nil))
		     (if (and ordered ordered-vars)
			 (progn
			   (setq current-key (car ordered-vars))
			   (setq current-value current-temp)
			   (set current-key current-value)
			   (setq return-value current-value)
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
			       (set current-key current-value)
			       (setq return-value current-value))
			 (setq ordered-vars (cdr ordered-vars))))
		     (message (concat 
			       "\nERROR:\n!: " 
			       class-name 
			       " too many variables specified;\n\n")))))
	       (setq data (cdr data)))
	     return-value))))
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
       (puthash key value class-vars)
       value))
    (class-name
     (lambda 
       ()
       class))
    (is-a? 
     (lambda (class-string)
 ;      (message "start is-a?")
       (let ((class-path (--- 'get-class-var 'class-path))
	     (class-symbol (intern class-string)))
;	 (print `(,class-symbol ,class-string ,class-path))
	 (if (memq class-symbol class-path)
	     (progn
;	       (message "True")
	       t)
	   (progn
;	     (message "True")
	     nil)))))
    (equiv?
     (lambda (other)
       (if (sos-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	     (maphash
	      (lambda (key value)
		(if (sos-instance? value)
		    (setq obj-check (-- value 'equiv? (-- other 'get key))))
		(if (not (or (equal (-- other 'get key) value) obj-check))
		    (setq y-or-n nil)))
	      self)
	     y-or-n)
	 nil)))
    (equal?
     (lambda (other)
       (if (--- 'object-instance? other)
	   (let ((y-or-n t)(obj-check nil))
	     (maphash
	      (lambda (key value)
		(if (--- 'object-instance? value)
		    (setq obj-check (-- value 'equiv? (-- other 'get key))))
		(if (not (or (equal (-- other 'get key) value) obj-check))
		    (setq y-or-n nil)))
	      self)
	     y-or-n)
	 nil)))
    (dynamic-message 
     (lambda (a-string)
       (-- (--- 'get-class-var 'dynamic-message) 'fire a-string)))
    (shallow-copy
     (lambda ()
       (let ((new-instance (! class)))
;	 (-- new-instance 'add-path-list (--- 'get-path-list))
	 (let ((var-list (--- 'get-class-var 'parameter-order)))
	   (while var-list
	     (-- new-instance 'set (car var-list) (eval (car var-list)))
	     (setq var-list (cdr var-list))))
	 new-instance)))))

; dynamic-message objects are used to tell the user that a long calculation is progressing.
; the fire method is called with a string as parameter, which sends a system message of the string
;  concatenated with a string of period(s). Subsequent calls increase the length of the periods by one,
;  until the length rises above period-length, then the periods string is reset to 1 period.
; A singleton instance exists as a class-var in object, along with a corresponding method
;  called "dynamic-message" which is called with a string as parameter; the method extracts 
;  the dynamic-message instance and runs "fire" with the string as data.

(sos-class "dynamic-message" "object"
 '((message " ")
   (periods ".")
   (max-period-length 100))
 '((fire
    (lambda (a-message)
      (if (string-equal message a-message)
	  (progn
	    (setq periods (concat periods "."))
	    (if (> (length periods) max-period-length)
		(setq periods ".")))
	(progn
	  (setq message a-message)
	  (setq periods ".")))
      (print `( ,(length periods)))
      (message (concat message periods))))))

(sos-set-class-var "object" 'dynamic-message (! "dynamic-message"))


; NOTES
; - simplicity of implementation of class structure requires that the entire system be rebuilt 
;   when anything that has been subclassed is changed (fortunately this does not appear to be an issue)
;   - ADV: if desireable, a system can be put in place to rebuild selective parts of a given tree, depending on timestamp.

; - data can be initialized through "!" calls by specifying a number of variables which are mapped to instance vars in order
; this allows us to abandon the "create function to build instances" practice
