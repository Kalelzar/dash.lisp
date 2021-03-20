(defpackage #:dash-system
  (:use #:cl #:asdf))
(in-package #:dash-system)

(defsystem :dash
  :name "Dash"
  :author "Kalelzar"
  :version "0.0.1"
  :description "A poor man's dash.el for Common Lisp"
  :serial t
  :components ((:file "package")
               (:file "sequential")
               (:file "dash")))
