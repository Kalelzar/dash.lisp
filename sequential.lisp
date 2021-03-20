(defpackage #:sequential
  (:use #:cl)
  (:export #:with-list))

(in-package #:sequential)

(defmacro with-list (list &rest forms)
  (if forms
      (progn
          (setf (cdr (last (car forms))) (list list)
                list (car forms)
                forms (cdr forms))
          `(with-list ,list ,@forms))
      list))
