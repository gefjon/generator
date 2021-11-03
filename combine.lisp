(uiop:define-package :generator/combine
  (:use :cl :generator/defs)
  (:import-from :alexandria
                #:array-index)
  (:shadow
   #:concatenate #:map #:substitute #:substitute-if #:remove #:remove-if #:remove-duplicates)
  (:export
   #:concatenate #:map #:substitute #:substitute-if #:enumerate #:zip #:take))
(in-package :generator/combine)

(declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))

(declaim (ftype (function (&rest generator) (values generator &optional))
                concatenate)
         (inline concatenate))
(defun concatenate (&rest generators)
  "Yield all the elements of each of the GENERATORS in order from left to right."
  (let* ((remaining generators))
    (labels ((concatenated ()
               (if remaining (handler-case (next (first remaining))
                               (done () (pop remaining) (concatenated)))
                   (done))))
      #'concatenated)))

(declaim (ftype (function ((function (&rest t) (values &rest t))
                           (generator &rest t))
                          (values generator &optional))
                map)
         (inline map))
(defun map (function generator)
  "Return a new generator which applies FUNCTION to the values of each element of GENERATOR, as by `multiple-value-call'"
  (generator ()
    (multiple-value-call function (next generator))))

(declaim (ftype (function (generator) (values (generator array-index &rest t) &optional))
                enumerate)
         (inline enumerate))
(defun enumerate (generator)
  (generator ((i -1))
    (declare (type (or array-index (eql -1)) i))
    ;; this odd ordering to avoid incrementing i if GENERATOR has finished
    (let ((vals (multiple-value-list (next generator))))
      (apply #'values (incf i) vals))))

(declaim (ftype (function (t (function (t) (values t &rest t))
                             (generator t &optional)
                             &key (:key (function (t) (values t &rest t))))
                          (generator t &optional))
                substitute-if)
         (inline substitute-if))
(defun substitute-if (new-element predicate generator &key (key #'identity))
  (generator ()
    (let ((original (next generator)))
      (if (funcall predicate (funcall key original)) new-element
          original))))

(declaim (ftype (function (t t (generator t &optional)
                             &key (:key (function (t) (values t &rest t)))
                             (:test (function (t t) (values t &rest t))))
                          (generator t &optional))
                substitute)
         (inline substitute))
(defun substitute (new-element old-element generator
                   &key (key #'identity) (test #'eql))
  (substitute-if new-element
                 (lambda (original) (funcall test old-element original))
                 generator
                 :key key))

(declaim (ftype (function (generator generator) (values generator &optional))
                zip)
         (inline zip))
(defun zip (left right)
  "Combine two generators into a new generator which yields as multiple values all the values from LEFT followed by all the values from RIGHT."
  (generator ()
    (multiple-value-call #'values (next left) (next right))))

(declaim (ftype (function (generator array-index) (values generator &optional))
                take)
         (inline take))
(defun take (generator n)
  (generator ((remaining n))
    (declare (type array-index remaining))
    (if (plusp remaining)
        (prog1 (next generator)
          (decf remaining))
        (done))))

(declaim (ftype (function ((function (&rest t) (values t &rest t)) generator)
                          (values generator &optional))
                remove-if)
         (inline remove-if))
(defun remove-if (predicate generator)
  (labels ((removed-generator ()
             (let* ((vals (multiple-value-list (next generator))))
               (if (apply predicate vals)
                   (removed-generator)
                   (values-list vals)))))
    #'removed-generator))

(declaim (ftype (function ((generator t &optional) &key (:test (or symbol function)))
                          (values (generator t &optional) &optional))
                remove-duplicates))
(defun remove-duplicates (generator &key (test #'eql))
  (let* ((ht (make-hash-table :test test)))
    (flet ((alreadyp (elt &aux (presentp (nth-value 1 (gethash elt ht))))
             (setf (gethash elt ht) t)
             presentp))
      (remove-if #'alreadyp generator))))
