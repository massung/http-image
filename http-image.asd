(defpackage :http-image-asd
  (:use :cl :asdf))

(in-package :http-image-asd)

(defsystem :http-image
  :name "http-image"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CAPI HTTP Image Pane for LispWorks."
  :serial t
  :components ((:file "http-image") (:file "http-image-pane"))
  :depends-on ("lexer" "http"))
