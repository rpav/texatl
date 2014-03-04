(defpackage :texatl
  (:use #:cl #:freetype2 #:freetype2-types #:alexandria #:laconic)
  (:export

   ;; font
   #:make-font-atlas #:make-font-atlas-files
   #:face-metrics #:glyph-index #:glyph-metrics #:glyph-kerning

   #:with-glyph-metrics #:do-texatl-string))
