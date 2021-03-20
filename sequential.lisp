(in-package #:sequential)

(defmacro with-list-1 (list &rest forms)
  (if forms
      (progn
          (setf (cdr (last (car forms))) (list list)
                list (car forms)
                forms (cdr forms))
          `(with-list-1 ,list ,@forms))
      list))

(defmacro with-list (list &rest forms)
  `(let ((original ,list))
     (declare (list original)
              (ignorable original))
     (with-list-1 ,list ,@forms)))
