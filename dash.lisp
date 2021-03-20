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

(defun -fold-left (init function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-rreduce function
            list
            init))

(defmacro --fold-left (init form list)
  (declare ((or list symbol) list))
  `(-fold-left ,init
               #'(lambda (acc it) ,form)
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

(defun -rmap-first (pred f acc list)
  (declare ((or function symbol) pred f))
  (declare ((or list symbol) list acc))
  (cond
    ((null list) acc)
    ((funcall pred (car list))
     (append acc
             (list (funcall f
                            (car list)))
             (cdr list)))
    (t (-rmap-first pred
                    f
                    (append acc
                            (list (car list)))
                    (cdr list)))))

(defun -map-first (pred f list)
  (declare ((or function symbol) pred f))
  (declare ((or list symbol) list))
  (-rmap-first pred f nil list))

(defmacro --map-first (pred form list)
  (declare ((or list symbol) list))
  `(-map-first #'(lambda (it) ,pred)
               #'(lambda (it) ,form)
               ,list))

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

(defun -map-last (pred f list)
  (declare ((or function symbol) pred f))
  (declare ((or list symbol) list))
  (reverse (-map-first pred f (reverse list))))

(defmacro --map-last (pred form list)
  (declare ((or list symbol) list))
  `(-map-last #'(lambda (it) ,pred)
               #'(lambda (it) ,form)
               ,list))

(defun -annotate (f list)
  (declare ((or function symbol) f))
  (declare ((or list symbol) list))
  (-zip-pair
   (-map f list)
   list))

(defmacro --annotate (form list)
  (declare ((or list symbol) list))
  `(-annotate (lambda (it) ,form)
              ,list))

(defun -mapcat (fn list)
  (declare ((or symbol function) fn))
  (declare ((or symbol list) list))
  (--fold-left nil
               (append acc
                       (funcall fn it))
               list))

(defmacro --mapcat (form list)
  (declare ((or symbol list) list))
  `(--fold-left nil
                (append acc
                        ,form)
                ,list))

(defun -concat (&rest list)
  (declare (list (or symbol list)))
  (--reduce (append acc it)
            list))

(defun -splice (pred fn list)
  (declare ((or symbol function) fn pred))
  (declare ((or symbol list) list))
  (-mapcat
   #'(lambda (it)
       (if (funcall pred it)
       (funcall fn it)
       (list it)))
   list))

(defmacro --splice (pred form list)
  (declare ((or list symbol) list))
  `(--mapcat
    (if ,pred
        ,form
        (list it))
    ,list))

(defun -splice-list (pred new-list list)
  (declare ((or symbol list) new-list list))
  (--splice (funcall pred it) new-list list))

(defmacro --splice-list (pred new-list list)
  (declare ((or symbol list) new-list list))
  `(--splice ,pred ,new-list ,list))


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

(defun -remove (function list)
  (declare ((or function symbol) function))
  (declare ((or list symbol) list))
  (-rfilter #'(lambda (x)
                (not (funcall function x)))
            list nil))

(defmacro --filter (form list)
  (declare ((or list symbol) list))
  `(-filter #'(lambda (it) ,form)
            ,list))

(defmacro --remove (form list)
  (declare ((or list symbol) list))
  `(-filter #'(lambda (it) (not ,form))
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

(defun -rzip-pair (acc list1 list2)
  (declare ((or list symbol) acc list1 list2))
  (let ((head (cons (car list1) (car list2))))
    (declare ((or list symbol) head))
    (if (and list1 list2)
      (-rzip-pair (append acc
                   (list head))
                  (cdr list1)
                  (cdr list2))
      acc)))

(defun -zip-pair (list1 list2)
  (declare ((or list symbol) list1 list2))
  (-rzip-pair nil list1 list2))

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
