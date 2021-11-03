(uiop:define-package :generator/produce
  (:use :cl :generator/defs)
  (:import-from :alexandria
                #:array-index)
  (:export
   #:generate-list #:generate-these #:generate-vector #:generate-range #:always

   #:make-generator))
(in-package :generator/produce)

(declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))

(declaim (ftype (function (list) (values (generator t &optional) &optional))
                generate-list)
         (inline generate-list))
(defun generate-list (list)
  "Generate elements of LIST in order left-to-right."
  (generator ((next list))
    (if next (pop next)
        (done))))

(declaim (ftype (function (&rest t) (values (generator t &optional) &optional))
                generate-these)
         (inline generate-these))
(defun generate-these (&rest elts)
  (generate-list elts))

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

Making any of these actions may cause a generator which had previously signaled `done' to produce
new elements, or do other weird stuff."
  (generator ((i 0))
    (declare (type array-index i))
    (if (< i (length vec)) (prog1 (aref vec i)
                             (incf i))
        (done))))

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
        (done))))

(declaim (ftype (function (&rest t) (values (generator &rest t) &optional))
                always))
(defun always (&rest values)
  "A generator which always returns VALUES, and never signals `done'"
  (generator ()
    (values-list values)))

;;;; a generic constructor, analogous to python's Iterable or rust's IntoIterator

(defgeneric make-generator (thing)
  (:documentation "Produce a generator from THING."))

(defmethod make-generator ((thing list))
  (generate-list thing))

(defmethod make-generator ((thing vector))
  (generate-vector thing))

(defmethod make-generator ((thing real))
  "Generate numbers to but not including REAL, starting from 0 with a step of 1"
  (generate-range :end thing))

