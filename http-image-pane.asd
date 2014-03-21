(defpackage :http-image-pane-asd
  (:use :cl :asdf))

(in-package :http-image-pane-asd)

(defsystem :http-image-pane
  :name "http-image-pane"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CAPI HTTP Image Output Pane for LispWorks."
  :serial t
  :components ((:file "http-image-pane"))
  :depends-on ("lexer" "http"))
