(defpackage #:utils/misc
  (:nicknames :misc-utils)
  (:use #:cl) 
  (:import-from :lol :defmacro! :group :alambda :self)
  (:import-from :alexandria :when-let :curry :once-only)
  (:export #:with-minmax #:macrolet_ #:with-if #:lift #:random-integer #:*epsilon* #:approx= #:rapprox= #:lapprox= #:cons-if #:nif #:cmp #:alias #:aliases #:mvbind #:on :this #:repeat #:maptimes #:mappl #:else #:some-values #:some-value #:any-equal #:any-string-equal #:split-if #:rest-if #:defcompose #:compose-method #:compose-method-if #:symbol-suffix #:symbol-number #:def-instance-p #:do-on #:pushnew-alist #:mv-mapcar #:take #:mapby #:unordered-equal #:product))

(in-package #:utils/misc)

(defparameter *epsilon* 0.0001)

(defmacro with-minmax ((x y) &body body)
  (once-only (x y)
    `(with-if (< ,x ,y)
              (let ((min (then ,x ,y))
                    (max (then ,y ,x)))
                ,@body))))

(defmacro macrolet_ (definitions body)
  "Macrolet with automatic ignore declarations for variables named _."
  `(macrolet
     ,(mapcar (lambda (d)
                (destructuring-bind (name lambda-list local-form) d
                  (if (find '_ lambda-list)
                      (list name lambda-list '(declare (ignore _)) local-form)
                      d)))
              definitions)
     ,body))

(defmacro with-if (test body)
  "Expands to (if test body-1 body-2), where body-1 has (then a b) replaced with a, and body-2 has (then a b) replaced with b."
  `(if ,test
       (macrolet_ ((then (then-form _) then-form)) ,body)
       (macrolet_ ((then (_ else-form) else-form)) ,body)))

(defun lift (f)
  (curry #'apply f))

(defun random-integer (min max)
  (+ (random (- max min)) min))

(defun approx= (x y &key (epsilon *epsilon*))
  (unless epsilon (error "Epsilon undefined"))
  (< (abs (- x y)) epsilon))

(defun rapprox= (x y &key (epsilon *epsilon*))
  (unless epsilon (error "Epsilon undefined"))
  (< 0 (- y x) epsilon))

(defun lapprox= (x y &key (epsilon *epsilon*))
  (unless epsilon (error "Epsilon undefined"))
  (< 0 (- x y) epsilon))

(defun cons-if (a b)
  "Return `(cons a b)` if `a`, otherwise `nil`"
  (when a (cons a b)))

; Hoyte 'LoL', p. 62 after Graham 'On Lisp
(defmacro! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))

(defmacro cmp (a b > = <)
  `(nif (- ,a ,b) ,> ,= ,<))

; See Paul Graham 'On Lisp', p. 214
(defmacro alias (new old)
  `(defmacro ,new (&rest args)
     `(,',old ,@args)))

(defmacro aliases (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(alias ,@pair))
               (group names 2))))

(alias mvbind multiple-value-bind)

(defmacro on (init-form &body body)
  "Bind `this` to `init-form`, evalutate `body` and then return `this`."
  `(let ((this ,init-form))
     ,@body
     this))

(defun repeat (f n)
  "Repeat `f` `n` times and collect results"
  (loop for i below n collect (funcall f)))

(defun maptimes (f n)
  "map `f` over the integers 0 <= i < n"
  (loop for i below n collect (funcall f i)))

(defun mappl (function list)
  "map `function` over `list` with `apply`"
  (mapcar (lambda (a) (apply function a)) list))

(defmacro else (var form)
  "If 'var' is nil return 'form' else return 'var'."
  `(if ,var ,var ,form))

(defmacro some-values (predicate &rest sequences)
  "Like 'some' execpt it returns the first values which satisfy 'predicate'."
  (let ((args (repeat #'gensym (length sequences))))
    `(some (lambda ,args (if (funcall ,predicate ,@args) (list ,@args))) ,@sequences)))

(defmacro! some-value (predicate sequence)
   "Like 'some' execpt it takes only one sequence and returns the first value which satisfies 'predicate'."
     `(some (lambda (,g!a) (if (funcall ,predicate ,g!a) ,g!a)) ,sequence))


(defmacro swap (place-1 place-2)
  "swap values of two places"
  (let ((g (gensym)))
    `(progn
       (setf ,g ,place-1)
       (setf ,place-1 ,place-2)
       (setf ,place-2 ,g))))

(defmacro any-equal (value &rest values)
  "Return t if `value` is equal to any of `values`"
  `(or ,@(mapcar (lambda (v) `(equal ,v ,value)) values)))

(defmacro any-string-equal (string &rest strings)
  `(or ,@(mapcar (lambda (s) `(string-equal ,s ,string)) strings)))

(defun split-if (predicate seq)
  "Split seq at the first element which satisfies predicate."
  (let ((i (position-if predicate seq)))
    (values (subseq seq 0 i)
            (subseq seq i))))

(defun rest-if (predicate seq)
  "The the tail of the list from the first element which satisfies predicate."
  (when-let ((i (position-if predicate seq)))
    (subseq seq i)))


;; See Paul Graham 'ANSI Common Lisp'
(defun compose (&rest fns)
  "Compose functions with multiple arguments.
   Taken from ANSI Common Lisp by Paul Graham."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))
;;

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

(defun symbol-number (symbol n)
  "Intern a new symbol suffixed with given number. E.g. symb-1"
  (symbol-suffix symbol (format nil "-~D" n)))

  

(defmacro def-instance-p (class)
  "Define a predicate, classname-p, to test if an object is an instance of class"
  (let ((name (symbol-suffix class "-P")))
    `(progn
       (defgeneric ,name (obj))
       (defmethod ,name ((obj ,class)) t)
       (defmethod ,name (obj) nil))))


(defmacro do-on (varlist end-test-form &body body)
  `(do ,(mapcar (lambda (var) (list (car var) (cdr var) (cdr var))) varlist) ,(list end-test-form nil) ,@body))

(defmacro! pushnew-alist (key value alist)
  `(let ((,g!e (find `(,,key) ,alist :key #'car)))
     (if ,g!e
         (pushnew ,value (cdr ,g!e))
         (push (list ,key ,value) ,alist))))

;; See Graham 'On Lisp', p. 216
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj `,`,propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props)))
 ;;   

(defmacro mv-mapcar (n function form)
  "Map `function` over `n` lists produced by `form`"
  (let ((syms (repeat #'gensym n)))
    `(mvbind ,syms ,form
         (mapcar ,function ,@syms))))

(defun mapby (n function list)
  "Map `function` over `list`, `n` elements at a time."
  (funcall
   (alambda (list acc)
     (if list
         (self (nthcdr n list) (append acc (list (apply function (take n list)))))
         acc))
   list nil))

(defun take (n list)
  "Take the first `n` elements of `list`."
  (subseq list 0 n))

(defun unordered-equal (list-1 list-2)
  "Return t if list-1 and list-2 have the same elements arranged in any order."
  (labels ((hash-sort (list)
             (sort (mapcar #'sxhash list) #'<)))
    (equal (hash-sort list-1) (hash-sort list-2))))

(defun product (&rest lists)
  "Return the Cartesian product of lists."
  (if (cdr lists)
      (mapcan (lambda (a)
                (mapcar (lambda (b) (cons a b))
                        (apply #'product (cdr lists))))
              (car lists))
      (mapcar #'list (car lists))))
