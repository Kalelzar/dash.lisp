(defpackage #:dash
    (:use #:cl #:sequential)
    (:export #:it
             #:acc
             #:it-index
             #:match-1
             #:match
             #:-reduce
             #:--reduce
             #:-map
             #:--map
             #:-filter
             #:--filter
             #:-each
             #:--each
             #:-rzip
             #:-zip
             #:-zip-fill
             #:-zip-with-index
             #:-map-indexed
             #:--map-indexed
             #:-each-indexed
             #:--each-indexed
             #:-map-when
             #:--map-when
             #:-each-when
             #:--each-when
             #:-all?
             #:--all?
             #:-some?
             #:--some?
             #:-car
             #:-cdr
             #:-cadr
             #:-cddr))

(in-package #:dash)

(defmacro match-1 (form &body clauses)
  (let* ((it-clauses
          (mapcar #'(lambda (clause)
                      (let* ((condition (car clause))
                             (call (car condition))
                             (args (cdr condition))
                             (newform (cons call
                                            (cons form
                                                  args))))
                        (cons newform
                              (cdr clause))))
                  clauses)))
    `(cond
       ,@it-clauses)))

(defmacro match (form &body clauses)
  `(let ((it ,form))
    (match-1 it ,@clauses)))

(defun -rreduce (function list acc)
  (declare ((or function symbol) function))
  (declare ((or symbol list) list))
  (if list
      (-rreduce function
                (cdr list)
                (funcall function
                         acc
                         (car list)))
      acc))

(defun -reduce (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-rreduce function
            (cdr list)
            (car list)))

(defmacro --reduce (form list)
  (declare ((or list symbol) list))
  `(-reduce #'(lambda (acc it) ,form)
            ,list))

(defun -map (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (mapcar function
          list))

(defmacro --map (form list)
  (declare ((or list symbol) list))
  `(-map #'(lambda (it) ,form)
         ,list))

(defun -rfilter (function list acc)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list acc))
  (if list
      (if (funcall function
                   (car list))
          (-rfilter function
                    (cdr list)
                    (append acc
                            (list (car list))))
          (-rfilter function
                    (cdr list)
                    acc))
      acc))

(defun -filter (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-rfilter function list nil))

(defmacro --filter (form list)
  (declare ((or list symbol) list))
  `(-filter #'(lambda (it) ,form)
            ,list))

(defun -each (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (dolist (elt list)
    (funcall function elt)))

(defmacro --each (form list)
  (declare ((or list symbol) list))
  `(-each #'(lambda (it) ,form)
         ,list))

(defun -rzip (acc lists)
  (declare ((or list symbol) acc lists))
  (let ((head (-car lists)))
    (declare ((or list symbol) head))
    (if (-all? #'identity lists)
      (-rzip (append acc
                   (list head))
         (-cdr lists))
      acc)))

(defun -zip (&rest lists)
  (declare ((or list symbol) lists))
  (-rzip nil lists))

(defun -rzip-fill (fill acc lists)
  (declare ((or list symbol) acc lists))
  (let ((head (-car lists)))
    (declare ((or list symbol) head))
    (if (-some? #'identity lists)
        (-rzip-fill fill
                    (with-list head
                      (--map-when (not it) fill)
                      (list)
                      (append acc))
                    (-cdr lists))
        acc)))

(defun -zip-fill (fill &rest lists)
  (declare ((or list symbol) lists))
  (-rzip-fill fill nil lists))

(defun -zip-with-index (list)
  (declare ((or list symbol) list))
  (-zip (loop for i from 0 below (list-length list) collect i)
        list))

(defun -map-indexed (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-map #'(lambda (pair)
            (destructuring-bind (index value) pair
              (declare (fixnum index))
              (funcall function index value)))
        (-zip-with-index list)))

(defmacro --map-indexed (form list)
  (declare ((or list symbol) list))
  `(-map #'(lambda (pair)
              (destructuring-bind (it-index it) pair
                (declare (fixnum it-index))
                ,form))
          (-zip-with-index ,list)))


(defun -each-indexed (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-each #'(lambda (pair)
             (destructuring-bind (index value) pair
               (declare (fixnum index))
               (funcall function index value)))
        (-zip-with-index list)))

(defmacro --each-indexed (form list)
  (declare ((or list symbol) list))
  `(-each #'(lambda (pair)
              (destructuring-bind (it-index it) pair
                (declare (fixnum it-index))
                ,form))
          (-zip-with-index ,list)))

(defun -map-when (pred function list)
  (declare ((or function symbol) pred function))
  (declare ((or list symbol) list))
  (-map
   #'(lambda (it)
       (if (funcall pred it)
       (funcall function it)
       it))
   list))

(defmacro --map-when (pred form list)
  (declare ((or list symbol) list))
  `(--map
    (if ,pred
        ,form
        it)
    ,list))


(defun -each-when (pred function list)
  (declare ((or function symbol) function pred))
  (declare ((or list symbol) list))
  (-each
   #'(lambda (it)
       (if (funcall pred it)
       (funcall function it)
       it))
   list))

(defmacro --each-when (pred form list)
  (declare ((or list symbol) list))
  `(--each
    (if ,pred
        ,form
        it)
    ,list))


(defun -all? (pred list)
  (declare ((or function symbol) pred))
  (declare ((or list symbol) list))
  (-reduce #'(lambda (acc it)
               (and acc it))
           (-map pred
                 list)))

(defmacro --all? (pred list)
  (declare ((or list symbol) list))
  `(--reduce (and acc it)
             (--map ,pred
                    ,list)))

(defun -some? (pred list)
  (declare ((or function symbol) pred))
  (declare ((or list symbol) list))
  (-reduce #'(lambda (acc it)
                (or acc it))
            (-map pred
                  list)))

(defmacro --some? (pred list)
  (declare ((or list symbol) list))
  `(--reduce (or acc it)
             (--map ,pred
                    ,list)))

(defun -car (list)
  (declare ((or list symbol) list))
  (-map #'car
        list))

(defun -cdr (list)
  (declare ((or list symbol) list))
  (-map #'cdr
        list))

(defun -cadr (list)
  (declare ((or list symbol) list))
  (-map #'cadr
        list))

(defun -cddr (list)
  (declare ((or list symbol) list))
  (-map #'cddr
        list))
