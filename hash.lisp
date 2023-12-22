(defpackage :utils/hash
  (:nicknames :hash-utils)
  (:use :cl :misc-utils)
  (:export :add-hash :get-hash :presentp :hash-table-keys :hash-table-values :key-present-error :key-absent-error))
(in-package :utils/hash)

(defun add-hash (hash-table &key (key (gensym)) value)
  (if (and key (gethash key hash-table))
      (error 'key-present-error))
  (setf (gethash key hash-table) value)
  key)

(defun get-hash (key hash-table)
  (or (gethash key hash-table)
      (error 'key-absent-error :key key)))

(defun presentp (key hash-table)
  (multiple-value-bind (v p) (gethash key hash-table)
    (declare (ignore v))
    p))

(define-condition key-present-error (error) ((key :initarg :key :reader key)))
(define-condition key-absent-error (error) ((key :initarg :key :reader key)))


