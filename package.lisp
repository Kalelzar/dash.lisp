(defpackage #:sequential
  (:use #:cl)
  (:export #:with-list
           #:original))

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
             #:-map-last
             #:-map-first
             #:--map-first
             #:--map-last
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
