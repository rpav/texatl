(defpackage :texatl-client.asdf
  (:use #:cl #:asdf))

(in-package :texatl-client.asdf)

(defsystem :texatl-client
  :description "Pure-CL texatl client system"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria :cl-conspack :laconic)
  :pathname "src"
  :serial t

  :components
  ((:file "package-client")
   (:file "font-client")))
