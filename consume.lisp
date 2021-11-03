(uiop:define-package :generator/consume
  (:use :cl :generator/defs)
  (:import-from :alexandria
                #:with-gensyms #:array-length)
  (:shadow #:length #:count #:count-if)
  (:export

   #:try-next
   #:call-with-generator-elements #:do-generator

   #:collect-to-list #:collect-to-vector

   #:skip #:length #:count #:count-if))
(in-package :generator/consume)

(declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))

(declaim (ftype (function (generator &rest t) (values boolean &rest t))
                try-next)
         (inline try-next))
(defun try-next (generator &rest end-values)
  "Advance GENERATOR, returning a boolean STILL-RUNNING-P and its values.

If GENERATOR yields values, return a primary value of T, and the generated values.

If GENERATOR signals `generator-done', return a primary value of NIL, and END-VALUES."
  (handler-case (values t (next generator))
    (done () (apply #'values nil end-values))))

(declaim (ftype (function (function generator &optional function) (values &rest t))
                call-with-generator-elements)
         (inline call-with-generator-elements))
(defun call-with-generator-elements (thunk generator &optional (end-thunk (constantly nil)))
  "Invoke THUNK on each element of GENERATOR as by `multiple-value-call'.

THUNK should accept as many elements as are produced by GENERATOR.

If END-THUNK is provided, call it with no arguments last and return its values."
  (handler-case (loop (multiple-value-call thunk (next generator)))
    (done () (funcall end-thunk))))

(defmacro do-generator ((vars generator &optional result) &body body)
  "Analogous to `dotimes'. Evaluate BODY for the  element VARS in GENERATOR, then return RESULT.

VARS should be either a lambda list or a symbol. Bare symbols will be bound to the primary value of each
element; lambda lists will be applied to all the values of each element."
  (let* ((thunk (etypecase vars
                  (list `(lambda ,vars
                           ,@body))
                  (symbol (with-gensyms (ignore)
                            `(lambda (,vars &rest ,ignore)
                               (declare (ignore ,ignore))
                               ,@body))))))
    `(call-with-generator-elements ,thunk ,generator (lambda () ,result))))

;;; collection into strict sequences

(declaim (ftype (function ((generator t &rest t)) (values list &optional))
                collect-to-list)
         (inline collect-to-list))
(defun collect-to-list (generator)
  (let* ((list nil))
    (do-generator (elt generator (nreverse list))
      (push elt list))))

(declaim (ftype (function ((generator t &rest t)
                           &key (:element-type t)
                           (:length-hint (or null array-length)))
                          (values vector &optional))
                collect-to-vector)
         (inline collect-to-vector))
(defun collect-to-vector (generator &key (element-type t) length-hint)
  (let* ((vec (make-array (or length-hint 0)
                          :fill-pointer 0
                          :element-type element-type
                          :adjustable t)))
    (do-generator (elt generator vec)
      (vector-push-extend elt vec))))


(declaim (ftype (function (generator array-length) (values &rest t))
                skip))
(defun skip (generator n)
  "Consume N elements from GENERATOR, returning the Nth element.

(skip GEN 0) is equivalent to (next gen)"
  (if (zerop n)
      (next generator)
      (progn (next generator)
             (skip generator (1- n)))))

(declaim (ftype (function (generator) (values array-length &optional))
                length))
(defun length (generator &aux (i 0))
  (declare (type array-length i))
  (do-generator (elt generator i)
    (declare (ignore elt))
    (incf i)))

(declaim (ftype (function ((function (&rest t) (values t &rest t))
                           generator)
                          (values array-length &optional))
                count-if)
         (inline count-if))
(defun count-if (predicate generator &aux (count 0))
  (declare (type array-length count))
  (do-generator ((&rest elt) generator count)
    (when (funcall predicate elt)
      (incf count))))

(declaim (ftype (function (t generator
                             &key (:test (function (t t) (values t &rest t))))
                          (values array-length &optional))
                count))
(defun count (item generator &key (test #'eql))
  (flet ((same-as-item-p (value)
           (funcall test item value)))
    (declare (dynamic-extent #'same-as-item-p))
    (count-if #'same-as-item-p generator)))
