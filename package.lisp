(uiop:define-package :generator/package
  (:nicknames :generator)
  (:use :cl :iterate)
  (:import-from :alexandria
                #:array-index #:with-gensyms)
  (:export
   ;; type and general constructor
   #:generator

   ;; iteration utilities
   #:generator-next #:generator-try-next #:do-generator

   ;; simple constructors
   #:generate-list #:generate-vector #:generate-range #:always

   ;; generic constructors
   #:make-generator

   ;; combinators
   #:generator-conc #:enumerate #:zip #:take))
(in-package :generator/package)

(deftype generator (&rest values)
  "A generator is a closure which takes no arguments and returns successive elements on each invocation, signaling `generator-done' when no elements remain.

Most generators yield one value at a time, and many operators treat them as such, but in the general case
generators may yield arbitrarily many values.

For the sake of terminology, we will say that on each invocation, a generator produces one /element/ which may
be one or more /values/."
  (let ((values (or values '(&rest t))))
    `(function () (values ,@values))))

(define-condition generator-done ()
  ())

(declaim (ftype (function () nil)
                generator-done))
(defun generator-done ()
  "Signal that a generator has finished"
  (error 'generator-done))

(defmacro generator (vars &body body)
  "Construct a generator which closes over VARS and evaluates BODY on each invocation.

VARS are treated as in `let*'."
  `(let* (,@vars)
     (flet ((generator-closure ()
              ,@body))
       #'generator-closure)))

(declaim (ftype (function (generator) (values &rest t))
                generator-next)
         (inline generator-next))
(defun generator-next (generator)
  "Advance GENERATOR, returning its next element, or signaling `generator-done' if none remain."
  (funcall generator))

(declaim (ftype (function (generator &rest t) (values boolean &rest t))
                generator-try-next)
         (inline generator-try-next))
(defun generator-try-next (generator &rest end-values)
  "Advance GENERATOR, returning a boolean STILL-RUNNING-P and its values.

If GENERATOR yields values, return a primary value of T, and the generated values.

If GENERATOR signals `generator-done', return a primary value of NIL, and END-VALUES."
  (handler-case (values t (generator-next generator))
    (generator-done () (apply #'values nil end-values))))

(defmacro-driver (for var in-generator generator)
  (let ((for (if generate 'generate 'for)))
    (with-gensyms (gen)
      `(progn
         (with ,gen = ,generator)
         (handler-case (,for ,var next (generator-next ,gen))
           (generator-done () (finish)))))))

(declaim (ftype (function (function generator &optional function) (values &rest t))
                call-with-generator-elements)
         (inline call-with-generator-elements))
(defun call-with-generator-elements (thunk generator &optional (end-thunk (constantly nil)))
  "Invoke THUNK on each element of GENERATOR as by `multiple-value-call'.

THUNK should accept as many elements as are produced by GENERATOR.

If END-THUNK is provided, call it with no arguments last and return its values."
  (handler-case (iter (multiple-value-call thunk (generator-next generator)))
    (generator-done () (funcall end-thunk))))

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

;;; simple constructors

(declaim (ftype (function (list) (values (generator t &optional) &optional))
                generate-list)
         (inline generate-list))
(defun generate-list (list)
  "Generate elements of LIST in order left-to-right."
  (generator ((next list))
    (if next (pop next)
        (generator-done))))

(declaim (ftype (function (vector) (values (generator t &optional) &optional))
                generate-vector)
         (inline generate-vector))
(defun generate-vector (vec)
  "Generate elements of VEC left-to-right.

If VEC has a fill pointer, only generate elements before the fill pointer.

The consequences are undefined if VEC is destructively modified during generation. This includes:
- Altering its contents via `setf' of `aref' or any other operator.
- Changing its fill pointer via `setf' of `fill-pointer', `vector-push', `vector-push-extend', or any other
  operator.
- For adjustable vectors, adjusting its dimensions or `displaced-to' array with `adjust-array',
  `vector-push-extend' or any other operator. This includes arrays which are not expressly adjustable, but are
  acutally adjustable per `array-adjustable-p'.

Making any of these actions may cause a generator which had previously signaled `generator-done' to produce
new elements, or do other weird stuff."
  (generator ((i 0))
    (declare (type array-index i))
    (if (< i (length vec)) (prog1 (aref vec i)
                             (incf i))
        (generator-done))))

(declaim (ftype (function (&key (:start real) (:end real) (:step (real (0))))
                          (values (generator real &optional) &optional))
                generate-range)
         (inline generate-range))
(defun generate-range (&key (start 0) (end 0) (step 1))
  "Generate numbers from START to but not including END in intervals of STEP.

END may be less than START, in which case iteration will go from most-positive to least-positive. Otherwise,
iteration will go from least-positive to most-positive.

STEP should always be positive, even for downwards iteration; if END is less than START, STEP will be
subtracted rather than added."
  (generator ((curr start)
              (compare (if (>= end start) #'< #'>))
              (actual-step (if (>= end start) step (- step))))
    (if (funcall compare curr end) (prog1 curr (incf curr actual-step))
        (generator-done))))

(declaim (ftype (function (&rest t) (values (generator &rest t) &optional))
                always))
(defun always (&rest values)
  "A generator which always returns VALUES, and never signals `generator-done'"
  (generator ()
    (values-list values)))

;;; generic constructor

(defgeneric make-generator (thing)
  (:documentation "Produce a generator from THING."))

(defmethod make-generator ((thing list))
  (generate-list thing))

(defmethod make-generator ((thing vector))
  (generate-vector thing))

(defmethod make-generator ((thing real))
  "Generate numbers to but not including REAL, starting from 0 with a step of 1"
  (generate-range :end thing))

;;; combinators

(declaim (ftype (function (&rest generator) (values generator &optional))
                generator-conc)
         (inline generator-conc))
(defun generator-conc (&rest generators)
  "Yield all the elements of each of the GENERATORS in order from left to right."
  (let* ((remaining generators))
    (labels ((concatenated ()
               (if remaining (handler-case (generator-next (first remaining))
                               (generator-done () (pop remaining) (concatenated)))
                   (generator-done))))
      #'concatenated)))

(declaim (ftype (function (generator) (values (generator array-index &rest t) &optional))
                enumerate)
         (inline enumerate))
(defun enumerate (generator)
  (generator ((i -1))
    (declare (type (or array-index (eql -1)) i))
    ;; this odd ordering to avoid incrementing i if GENERATOR has finished
    (let ((vals (multiple-value-list (generator-next generator))))
      (apply #'values (incf i) vals))))

(declaim (ftype (function (generator generator) (values generator &optional))
                zip)
         (inline zip))
(defun zip (left right)
  "Combine two generators into a new generator which yields as multiple values all the values from LEFT followed by all the values from RIGHT."
  (generator ()
    (multiple-value-call #'values (generator-next left) (generator-next right))))

(declaim (ftype (function (generator array-index) (values generator &optional))
                take)
         (inline take))
(defun take (generator n)
  (generator ((remaining n))
    (declare (type array-index remaining))
    (if (plusp remaining)
        (prog1 (generator-next generator)
          (decf remaining))
        (generator-done))))
