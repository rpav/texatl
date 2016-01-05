(defpackage :texatl.cl
  (:use #:cl #:alexandria #:laconic)
  (:export

   ;; font
   #:texatl-font

   #:face-metrics #:glyph-index #:glyph-metrics #:glyph-kerning
   #:with-glyph-metrics #:do-texatl-string #:texatl-string-width

   ;; sprite
   #:texatl-spritesheet #:sprite #:frame-count #:with-sprite
   #:mapsheet))
