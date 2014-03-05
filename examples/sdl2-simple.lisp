(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "sdl2kit")
  (asdf:load-system "texatl"))

(defpackage :texatl.sdl2.ex
  (:use #:cl #:alexandria #:sdl2.kit #:texatl.cl))

(in-package :texatl.sdl2.ex)

(defparameter *simple-vertex-shader* "
varying vec2 texture_coordinate;
void main() {
    texture_coordinate = vec2(gl_MultiTexCoord0);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
")

(defparameter *programs*
  `((:glyph
     (:uniforms :texid)
     (:shaders
      :vertex-shader ,*simple-vertex-shader*
      :fragment-shader "
#version 120

varying vec2 texture_coordinate;
uniform sampler2D texid;

void main() {
    gl_FragColor = texture2D(texid, texture_coordinate);
}
"))
    (:color
     (:uniforms :color)
     (:shaders
      :vertex-shader ,*simple-vertex-shader*
      :fragment-shader "
#version 120
uniform vec4 color;

void main() {
    gl_FragColor = color;
}
"))))

(defclass hello-window-2 (gl-window)
  ((text :initform "(hello world)" :initarg :text)
   (texid :initform nil)
   (programs :initform nil)
   (surface :initform nil)
   (font :initform nil)))

(defmethod initialize-instance ((window hello-window-2)
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

(defun make-quad (x0 y0 x1 y1 u0 v0 u1 v1)
  (gl:with-primitives :quads
    (gl:tex-coord u0 v0) (gl:vertex x0 y0)
    (gl:tex-coord u0 v1) (gl:vertex x0 y1)
    (gl:tex-coord u1 v1) (gl:vertex x1 y1)
    (gl:tex-coord u1 v0) (gl:vertex x1 y0)))

(defmethod render ((window hello-window-2))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer)
  (with-slots (text font programs) window
    (sdl2.kit:use-program programs :glyph)
    (do-texatl-string (text x0 y0 x1 y1 u0 v0 u1 v1
                       :tex-width 128 :tex-height 128) font
      (make-quad x0 y0 x1 y1 u0 v0 u1 v1))

    ;; You could trivially implement word wrapping by splitting a
    ;; string into words and calculating the width of each:
    (let ((x (texatl-string-width text font)))
      (sdl2.kit:use-program programs :color)
      (sdl2.kit:uniformf programs :color 0.0 0.0 1.0 1.0)
      (make-quad x 0 (+ x 2) 20 0 0 1 1))))

(defmethod keyboard-event ((window hello-window-2) st ts r ks)
  (when (eq :scancode-escape (sdl2:scancode ks))
    (sdl2.kit:close-window window)))

;;; (sdl2.kit:start)
;;; (make-instance 'hello-window-2 :font-file "/usr/share/fonts/corefonts/times.ttf")
