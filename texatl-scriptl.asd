(defpackage :texatl-scriptl.asdf
  (:use #:cl #:asdf))

(in-package :texatl-scriptl.asdf)

(defsystem :texatl-scriptl
  :description "ScriptL commands for texatl"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:texatl :scriptl :unix-options)
  :pathname "src"
  :serial t

  :components
  ((:file "scriptl")))
