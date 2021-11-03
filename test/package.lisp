(uiop:define-package :generator/test/package
  (:nicknames :generator/test)
  (:use)
  (:mix :generator :fiveam :cl)
  (:local-nicknames (:gen :generator))
  (:export #:generator-suite))
(in-package :generator/test/package)

(def-suite generator-suite)

(def-test list-identity (:suite generator-suite)
  (flet ((generator-identity (list)
           (gen:collect-to-list (gen:generate-list list))))
    (for-all ((list (gen-list)))
      (is (equal list (generator-identity list))))))

(def-test string-identity (:suite generator-suite)
  (flet ((generator-identity (string)
           (gen:collect-to-vector (gen:generate-vector string)
                              :element-type (array-element-type string)
                              :length-hint (cl:length string))))
    (for-all ((string (gen-string)))
      (is (equal string (generator-identity string))))))

(def-test simple-zip (:suite generator-suite)
  (for-all ((list1 (gen-list :length (constantly 10)))
            (list2 (gen-list :length (constantly 10))))
    (is (equal (mapcar #'list list1 list2)
               (collect-to-list (map #'list
                                     (zip (generate-list list1)
                                          (generate-list list2))))))))
