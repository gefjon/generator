(uiop:define-package generator/package
  (:nicknames generator)
  (:mix cl iterate)
  (:import-from gefjon-utils
                func define-class)
  (:import-from cl-cont
                with-call/cc let/cc)
  (:import-from alexandria
                once-only with-gensyms)
  (:export generator generator-yield generator-next))
(in-package generator)

(deftype generator (&optional output-type)
  `(func () ,output-type))

(defmacro generator-yield (thing)
  (declare (ignore thing))
  (error "Cannot `generator-yield' outside of a `generator'!"))

(define-class %generator-done ()
  :condition t)

(define-class %generator-yield ((vals list))
  :condition t)

(defmacro generator (vars &body body)
  `(let* (,@vars
          next-body)
     (flet ((generator-closure ()
              (handler-case
                  (funcall (or next-body
                               (lambda ()
                                 (with-call/cc
                                   (macrolet ((yield (thing)
                                                `(let/cc k
                                                   (setf next-body k)
                                                   (error '%generator-yield :vals (multiple-value-list ,thing)))))
                                     ,@body)
                                   (error '%generator-done)))))
                (%generator-yield (condition) (values-list (vals condition))))))
       #'generator-closure)))

(defun generator-next (generator)
  (funcall generator))

(defmacro-driver (for var in-generator generator)
  (let ((for (if generate 'generate 'for)))
    (with-gensyms (gen)
      `(progn
         (with ,gen = ,generator)
         (handler-case (,for ,var next (generator-next ,gen))
           (%generator-done () (finish)))))))

(defmacro do-generator ((var generator &optional (result '(values))) &body body)
  `(iter
     (for ,var in-generator ,generator)
     (finally (return ,result))
     ,@body))
