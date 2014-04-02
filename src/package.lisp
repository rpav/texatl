(defpackage :texatl
  (:use #:cl #:freetype2 #:freetype2-types #:alexandria #:laconic)
  (:export

   ;; font
   #:make-font-atlas #:make-font-atlas-files

   ;; sprites
   #:make-sprite-atlas #:make-sprite-atlas-files))
