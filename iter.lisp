(defpackage :utils/iter
  (:nicknames :iter-utils)
  (:shadow :mean)
  (:shadowing-import-from :num-utils :sum)
  (:use :cl :alexandria :iterate :num-utils)
  (:export :mean :vector-sum :vector-mean))

(in-package :utils/iter)

(defmacro-clause (mean x &optional weight (w 1) into var)
  (with-gensyms (num div)
    `(progn (reducing ,x by (lambda (acc x) (+ acc (* ,w x))) initial-value 0 into ,num)
            (summing ,w into ,div)
            ,(if var `(for ,var = (/ ,num ,div))
                     `(finally (return (/ ,num ,div)))))))

(defmacro-clause (vector-sum v &optional into var) `(reducing ,v by #'e+ initial-value 0 into ,var))
(defmacro-clause (vector-multiply v &optional into var) `(reducing ,v by #'e* initial-value 0 into ,var))

(defmacro-clause (vector-mean v &optional weight (w 1) into var)
  (with-gensyms (num div)
    `(progn (reducing ,v by (lambda (acc v) (e+ acc (e* ,w v))) initial-value 0 into ,num)
            (summing ,w into ,div)
            ,(if var `(for ,var = (e/ ,num ,div))
                     `(finally (return (e/ ,num ,div)))))))
