(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "sdl2kit"))

(defpackage :texatl.sdl2.ex
  (:use #:cl #:alexandria #:sdl2.kit))

(in-package :texatl.sdl2.ex)

(defclass hello-window (gl-window)
  ((text :initform "(hello world)" :initarg :text)
   (texid :initform nil)
   (program :initform nil)
   (surface :initform nil)
   (face-metrics :initform nil)
   (glyph-index :initform nil)
   (glyph-metrics :initform nil)
   (glyph-kerning :initform nil)))

(defmethod initialize-instance ((window hello-window)
                                &key font-file (point-size 18) (dpi 72)
                                (w 800) (h 600)
                                &allow-other-keys)
  (call-next-method)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:ortho 0 w h 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  ;; Load font:
  (with-slots (surface face-metrics glyph-index glyph-metrics
               glyph-kerning) window
    (multiple-value-bind (s fm gi gm gk)
        (texatl:make-font-atlas 128 128 font-file point-size
                                :dpi dpi)
      (setf surface s
            face-metrics fm
            glyph-index gi
            glyph-metrics gm
            glyph-kerning gk)))

  ;; Upload the texture:
  (let ((texname (car (gl:gen-textures 1))))
    )
  )

(defmethod render ((window hello-window))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer)
  (with-slots (text glyph-index glyph-metrics glyph-kerning)
      window
    (loop for c across text)))

;;; (sdl2.kit:start)
;;; (make-instance 'hello-window :font-file "/usr/share/fonts/corefonts/times.ttf")
