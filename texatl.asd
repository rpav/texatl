(defpackage :texatl.asdf
  (:use #:cl #:asdf))

(in-package :texatl.asdf)

(defsystem :texatl
  :description "Texture atlas generation, including fonts"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria :cl-freetype2 :cl-cairo2 :texatl-client)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "font")))
