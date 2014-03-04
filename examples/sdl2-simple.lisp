(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "sdl2kit")
  (asdf:load-system "texatl"))

(defpackage :texatl.sdl2.ex
  (:use #:cl #:alexandria #:sdl2.kit #:texatl))

(in-package :texatl.sdl2.ex)

(defparameter *programs*
  '((:glyph
     (:uniforms :texid)
     (:shaders
      :vertex-shader "
varying vec2 texture_coordinate;
void main() {
    texture_coordinate = vec2(gl_MultiTexCoord0);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
"
      :fragment-shader "
#version 120

varying vec2 texture_coordinate;
uniform sampler2D texid;

void main() {
    gl_FragColor = texture2D(texid, texture_coordinate);
}
"))))

(defclass hello-window (gl-window)
  ((text :initform "(hello world)" :initarg :text)
   (texid :initform nil)
   (programs :initform nil)
   (surface :initform nil)
   (font :initform nil)))

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

  (with-slots (surface programs texid font) window
    ;; Load font:
    (multiple-value-bind (s f)
        (texatl:make-font-atlas 128 128 font-file point-size
                                :dpi dpi)
      (setf surface s
            font f))

    ;; Build shaders:
    (setf programs (sdl2.kit:compile-shader-dictionary *programs*))

    ;; Upload the texture:
    (let ((texname (car (gl:gen-textures 1)))
          (ptr (cairo:image-surface-get-data surface :pointer-only t)))
      (sdl2.kit:use-program programs :glyph)
      (setf texid texname)
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d texname)
      (sdl2.kit:uniformi programs :texid 0)

      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-image-2d :texture-2d 0 :rgba 128 128
                       0 :bgra :unsigned-byte ptr))))

(defun make-quad (x y w h u0 v0 u1 v1)
  (gl:with-primitives :quads
    (gl:tex-coord u0 v0)
    (gl:vertex x y)
    (gl:tex-coord u0 v1)
    (gl:vertex x (+ y h))
    (gl:tex-coord u1 v1)
    (gl:vertex (+ x w) (+ y h))
    (gl:tex-coord u1 v0)
    (gl:vertex (+ x w) y)))

(defmacro with-glyph-metrics ((x y width height advance left top)
                              glyph-metrics
                              &body body)
  (once-only (glyph-metrics)
    `(let ((,x (aref ,glyph-metrics 0))
           (,y (aref ,glyph-metrics 1))
           (,width (aref ,glyph-metrics 2))
           (,height (aref ,glyph-metrics 3))
           (,advance (aref ,glyph-metrics 4))
           (,left (aref ,glyph-metrics 5))
           (,top (aref ,glyph-metrics 6)))
       ,@body)))

(defmethod render ((window hello-window))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer)
  (with-slots (text font) window
    (with-slots (face-metrics glyph-index glyph-metrics glyph-kerning)
        font
      (let ((cx 0.0)
            (scale (/ 1.0 128))
            (max-ascender (aref face-metrics 0)))
        (loop for b = nil then c
              for c across text
              as index = (gethash c glyph-index)
              as met = (aref glyph-metrics index)
              as kerning = (or (cdr (assoc (cons b c) glyph-kerning
                                           :test 'equal))
                               0.0)
              do (with-glyph-metrics (x y w h adv left top) met
                   (let* ((u0 (* x scale))
                          (v0 (* y scale))
                          (u1 (+ u0 (* w scale)))
                          (v1 (+ v0 (* h scale))))
                     (make-quad (round (+ cx left kerning))
                                (round (- max-ascender top))
                                w h
                                u0 v0 u1 v1))
                   (incf cx (+ adv kerning))))))))

(defmethod keyboard-event ((window hello-window) st ts r ks)
  (when (eq :scancode-escape (sdl2:scancode ks))
    (sdl2.kit:close-window window)))

;;; (sdl2.kit:start)
;;; (make-instance 'hello-window :font-file "/usr/share/fonts/corefonts/times.ttf")
