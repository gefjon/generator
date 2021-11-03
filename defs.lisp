;;;; core definitions:
;;; - what is a generator?
;;; - how do i construct a generator?
;;; - how do i consume a generator?
(uiop:define-package :generator/defs
  (:use :cl)
  (:export #:generator #:next #:done))
(in-package :generator/defs)

(deftype generator (&rest values)
  "A generator is a closure which takes no arguments and returns successive elements on each invocation, signaling `done' when no elements remain.

Most generators yield one value at a time, and many operators treat them as such, but in the general case
generators may yield arbitrarily many values.

For the sake of terminology, we will say that on each invocation, a generator produces one /element/ which may
be one or more /values/."
  (let ((values (or values '(&rest t))))
    `(function () (values ,@values))))

(define-condition done ()
  ())

(declaim (ftype (function () nil) done)
         (inline done))
(defun done ()
  "Signal that a generator has finished"
  (error 'done))

(defmacro generator (vars &body body)
  "Construct a generator which closes over VARS and evaluates BODY on each invocation.

VARS are treated as in `let*'."
  `(let* (,@vars)
     (flet ((generator-closure ()
              ,@body))
       #'generator-closure)))

(declaim (ftype (function (generator) (values &rest t)) next)
         (inline next))
(defun next (generator)
  "Advance GENERATOR, returning its next element, or signaling `done' if none remain."
  (funcall generator))
