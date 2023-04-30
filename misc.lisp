(defpackage #:misc
  (:use #:cl)
  (:export #:on #:carr #:any-equal #:any-string-equal #:compose #:defcompose #:compose-method #:compose-method-if #:def-instance-p #:do-on))
(in-package #:misc)

(defun carr (obj)
  (if (consp obj) (car obj) obj))

(defmacro on ((var init-form) &body body)
  `(let ((,var ,init-form))
     ,@body
     ,var ))

(defmacro swap (place-1 place-2)
  "swap values of two places"
  (let ((g (gensym)))
    `(progn
       (setf ,g ,place-1)
       (setf ,place-1 ,place-2)
       (setf ,place-2 ,g))))

(defmacro any-equal (value &rest values)
  `(or ,@(mapcar (lambda (v) `(equal ,v ,value)) values)))

(defmacro any-string-equal (string &rest strings)
  `(or ,@(mapcar (lambda (s) `(string-equal ,s ,string)) strings)))

(defun split-if (predicate seq)
  "Split seq at the first element which satisfies predicate."
  (let ((i (position-if predicate seq)))
    (values (subseq seq 0 i)
	    (subseq seq i))))

(defun compose (&rest fns)
  "Compose functions with multiple arguments.
   Taken from ANSI Common Lisp by Paul Graham."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

(defmacro defcompose (name &rest functions)
  "Define a function NAME comprising the composition of FUNCTIONS"
  (let ((args (gensym)))
    `(defun ,name (&rest ,args)
       (apply (compose ,@functions) ,args))))


(defmacro compose-method (method &rest args)
  "Specialize method on class by composing funtion with it.
   Syntax: (compose-method {qualifier} specialized-lambda-list function)"
  (multiple-value-bind (qualifiers args) (split-if #'listp args)
    (let ((lambda-list (car args))
	  (function (cadr args)))
      `(defmethod ,method ,@qualifiers ,lambda-list
	 (,function (call-next-method))))))

(defmacro compose-method-if (method &rest args)
  "Specialize method on class by composing funtion with it.
   Fall back to next-method if function returns nil
   Syntax: (compose-method {qualifier} specialized-lambda-list function)"
  (multiple-value-bind (qualifiers args) (split-if #'listp args)
    (let ((a (gensym))
	  (b (gensym))
	  (lambda-list (car args))
	  (function (cadr args)))
      `(defmethod ,method ,@qualifiers ,lambda-list
	 (let* ((,a (call-next-method))
		(,b (,function ,a)))
	   (or ,b ,a))))))

(defun symbol-suffix (symbol string)
  "Intern a new symbol with given suffix"
  (intern (concatenate 'string (symbol-name symbol) string)))

(defmacro def-instance-p (class)
  "Define a predicate, classname-p, to test if an object is an instance of class"
  (let ((name (symbol-suffix class "-P")))
    `(progn
       (defgeneric ,name (obj))
       (defmethod ,name ((obj ,class)) t)
       (defmethod ,name (obj) nil))))


(defmacro do-on (varlist end-test-form &body body)
  `(do ,(mapcar (lambda (var) (list (car var) (cdr var) (cdr var))) varlist) ,(list end-test-form nil) ,@body))
