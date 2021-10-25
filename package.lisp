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
   #:generate-list #:generate-vector #:generate-range

   ;; generic constructors
   #:make-generator

   ;; combinators
   #:generator-conc))
(in-package :generator/package)

(deftype generator (&optional (output-type t))
  "A generator is a closure which takes no arguments and returns successive elements on each invocation, signaling `generator-done' when no elements remain."
  `(function () (values ,output-type &optional)))

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

(declaim (ftype (function (generator) (values t &optional))
                generator-next))
(defun generator-next (generator)
  "Advance GENERATOR, returning its next element, or signaling `generator-done' if none remain."
  (funcall generator))

(declaim (ftype (function (generator &optional t) (values t boolean &optional))
                generator-try-next))
(defun generator-try-next (generator &optional end-value)
  "Advance GENERATOR, returning its next element, or return END-VALUE if none remain.

Return T as a secondary value if an element from GENERATOR was returned, or NIL if END-VALUE was returned
because GENERATOR signaled `generator-done'."
  (handler-case (values (generator-next generator) t)
    (generator-done () (values end-value nil))))

(defmacro-driver (for var in-generator generator)
  (let ((for (if generate 'generate 'for)))
    (with-gensyms (gen)
      `(progn
         (with ,gen = ,generator)
         (handler-case (,for ,var next (generator-next ,gen))
           (generator-done () (finish)))))))

(defmacro do-generator ((var generator &optional (result '(values))) &body body)
  "Analogous to `dotimes'. Evaluate BODY for each element VAR in GENERATOR, then return RESULT."
  `(iter
     (for ,var in-generator ,generator)
     (finally (return ,result))
     ,@body))

;;; simple constructors

(declaim (ftype (function (list) (values generator &optional))
                generate-list)
         (inline generate-list))
(defun generate-list (list)
  "Generate elements of LIST in order left-to-right."
  (generator ((next list))
    (if next (pop next)
        (generator-done))))

(declaim (ftype (function (vector) (values generator &optional))
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
                          (values (generator real) &optional))
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
                generator-conc))
(defun generator-conc (&rest generators)
  "Yield all the elements of each of the GENERATORS in order from left to right."
  (let* ((remaining generators))
    (labels ((concatenated ()
               (if remaining (handler-case (generator-next (first remaining))
                               (generator-done () (pop remaining) (concatenated)))
                   (generator-done))))
      #'concatenated)))
