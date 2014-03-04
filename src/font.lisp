;;;
;;; A lot of this is taken directly from the cairo demo for
;;; user-font-faces.  That wasn't particularly efficient and neither
;;; is this, but in this case it's even less critical: the odds of
;;; rendering a character more than once or so are very low.
;;;
(in-package :texatl)

(defparameter *default-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[] ")

(defclass ft-user-font (cairo:user-font-face)
  ((ft-face :initarg :ft-face :accessor ft-user-font-face)
   (size :initarg :size :accessor ft-user-font-size)
   (dpi :initarg :dpi :accessor ft-user-font-dpi)))

(defclass texatl-font ()
  ((face-metrics :initform nil :initarg :face-metrics :reader texatl-face-metrics)
   (glyph-index :initform nil :initarg :glyph-index :reader texatl-glyph-index)
   (glyph-metrics :initform nil :initarg :glyph-metrics :reader texatl-glyph-metrics)
   (glyph-kerning :initform nil :initarg :glyph-kerning :reader texatl-glyph-kerning)))

(defmethod conspack:encode-object ((font texatl-font) &key &allow-other-keys)
  (with-slots (face-metrics glyph-index glyph-metrics glyph-kerning)
      font
    (alist :face-metrics face-metrics
           :glyph-index glyph-index
           :glyph-metrics glyph-metrics
           :glyph-kerning glyph-kerning)))

(defmethod conspack:decode-object ((class (eql 'texatl-font)) alist &key &allow-other-keys)
  (alist-bind (face-metrics glyph-index glyph-metrics glyph-kerning) alist
    (make-instance 'texatl-font
      :face-metrics face-metrics
      :glyph-index glyph-index
      :glyph-metrics glyph-metrics
      :glyph-kerning glyph-kerning)))

(defvar *font* nil)
(defvar *face-metrics* nil)

(defun glyph-index-load-render (face char vertical-p)
  "Return the glyph index instead of a bitmap"
  (multiple-value-bind (bitmap advance top left)
      (default-load-render face char vertical-p)
    (declare (ignore bitmap))
    (values (get-char-index face char) advance top left)))

(defun init-user-font (scaled-font ctx extents)
  (declare (ignore ctx scaled-font))
  (multiple-value-bind (x-ppem y-ppem x-scale y-scale ascender descender
                        height max-advance)
      (face-metrics (ft-user-font-face *font*))
    (declare (ignore x-ppem y-ppem x-scale y-scale))
    (setf *face-metrics*
          (make-array 4
                      :initial-contents
                      (list ascender descender height max-advance)))
    (setf (cairo:font-ascent extents) ascender
          (cairo:font-descent extents) descender
          (cairo:font-height extents) height
          (cairo:font-max-x-advance extents) max-advance
          (cairo:font-max-y-advance extents) max-advance)))

(defun render-user-glyph (scaled-font glyph ctx text-extents)
  (declare (ignore text-extents))
  (let* ((user-font (cairo:scaled-font-face scaled-font))
         (face (ft-user-font-face user-font)))
    (cairo:with-context (ctx)
      (ft2:load-glyph face glyph)
      (let* ((glyphslot (render-glyph face))
             (bitmap (bitmap-convert (ft-glyphslot-bitmap glyphslot) 4))
             (width (ft-bitmap-width bitmap))
             (height (ft-bitmap-rows bitmap))
             (stride (ft-bitmap-pitch bitmap))
             (ptr (ft-bitmap-buffer bitmap))
             (scale (ft-user-font-size user-font))
             (mask (cairo:create-image-surface-for-data ptr :a8 width height stride)))
        (cairo:scale (/ 1.0 scale) (/ 1.0 scale))
        (cairo:mask-surface mask 0 0)))))

(defun render-to-glyph-array (texatl-font glyph-array string offset width y face)
  "=> STRING-OFFSET, MAX-HEIGHT

Populate GLYPH-ARRAY from STRING with maximum width WIDTH.  Note this
does NOT use DO-STRING-RENDER and does not output glyphs with proper
spacing.  Each glyph needs to be entirely in its own cell.

Returns the index of the next character in STRING that should be
rendered, if all characters are not rendered, and the MAX-HEIGHT of
the rendered row."
  (cairo:glyph-array-reset-fill glyph-array)
  (let ((cur-x 0d0)
        (max-height 0))
    (with-slots (glyph-index glyph-metrics glyph-kerning) texatl-font
      (loop for c across (subseq string offset)
            for i from offset
            as glyph = (progn (load-char face c)
                              (render-glyph face))
            as bitmap = (bitmap-convert (ft-glyphslot-bitmap glyph) 4)
            as glyph-width = (ft-bitmap-width bitmap)
            do (when (> (+ cur-x glyph-width) width)
                 (return (values i max-height)))
               ;; Store metrics
               (setf (gethash c glyph-index) i)
               (setf (aref glyph-metrics i)
                     (make-array 7
                                 :initial-contents
                                 (list
                                  (truncate cur-x) y glyph-width
                                  (ft-bitmap-rows bitmap)
                                  (get-loaded-advance face nil)
                                  (ft-glyphslot-bitmap-left glyph)
                                  (ft-glyphslot-bitmap-top glyph))))
               (loop for d across string
                     as kern = (get-kerning face c d)
                     do (unless (= 0.0 kern)
                          (push (cons (cons c d) kern)
                                glyph-kerning)))
               ;; Add to array
               (cairo:glyph-array-add glyph-array (get-char-index face c) cur-x y)
               (incf cur-x glyph-width)
               (setf max-height (max max-height (ft-bitmap-rows bitmap)))))))

(defun make-font-atlas (width height font-name point-size &key (dpi 72) (string *default-characters*))
  (let* ((surface (cairo:create-image-surface :argb32 width height))
         (ctx (cairo:create-context surface)))
    (cairo:with-context (ctx)
      (let ((face (ft2:new-face font-name)))
        (let ((*font* (make-instance 'ft-user-font
                        :ft-face face
                        :size point-size
                        :dpi dpi
                        :init 'init-user-font
                        :render-glyph 'render-user-glyph))
              (*face-metrics* nil)
              (texatl-font (make-instance 'texatl-font
                             :glyph-index (make-hash-table)
                             :glyph-metrics (make-array (length string))))
              (ftm (cairo:make-trans-matrix :xx (coerce point-size 'double-float)
                                            :yy (coerce point-size 'double-float)))
              (ctm (cairo:make-trans-matrix))
              (opt (cairo:create-font-options))
              (glyph-array (cairo:make-glyph-array (length string))))
          (let ((scaled-font (cairo:create-scaled-font *font* ftm ctm opt)))
            (cairo:set-source-rgb 0 0 0)
            (cairo:paint)

            (cairo:set-source-rgb 1 1 1)
            (ft2:set-char-size face (* point-size 64) 0 dpi dpi)
            (cairo:set-font scaled-font)

            (let ((row-height 0)
                  (y 0)
                  (i 0))
              (loop while i do
                (multiple-value-setq (i row-height)
                  (render-to-glyph-array texatl-font glyph-array string i width y face))
                (when row-height (incf y row-height))
                (cairo:show-glyphs glyph-array))))

          (cairo:destroy ctx)

          (with-slots (face-metrics) texatl-font
            (setf face-metrics *face-metrics*))

          (values surface texatl-font))))))

(defun make-font-atlas-files (png-filename metrics-filename
                              width height font-name point-size
                              &key (dpi 72) (string *default-characters*))
  (multiple-value-bind (surface texatl-font)
      (make-font-atlas width height font-name point-size :dpi dpi :string string)
    (cairo:surface-write-to-png surface png-filename)
    (cairo:destroy surface)
    (with-open-file (stream metrics-filename :element-type '(unsigned-byte 8)
                                             :direction :output
                                             :if-exists :supersede)
      (conspack:encode texatl-font :stream stream)))
  (values))

(defmacro with-glyph-metrics ((x y width height advance left top)
                              glyph-metrics
                              &body body)
  "Bind the metrics for a specific provided glyph metric vector"
  (once-only (glyph-metrics)
    `(let ((,x (aref ,glyph-metrics 0))
           (,y (aref ,glyph-metrics 1))
           (,width (aref ,glyph-metrics 2))
           (,height (aref ,glyph-metrics 3))
           (,advance (aref ,glyph-metrics 4))
           (,left (aref ,glyph-metrics 5))
           (,top (aref ,glyph-metrics 6)))
       ,@body)))

(defmacro do-texatl-string ((string x0 y0 x1 y1 u0 v0 u1 v1
                             &key (tex-width 1) (tex-height 1))
                            texatl-font &body body)
  "Walk `STRING` and provide coordinates for each glyph.  Note that
these assume an *upper-left origin*.  If `TEX-WIDTH` and `TEX-HEIGHT`
are not provided, texture coordinates will be returned in
pixels (scale of 1)."
  (with-gensyms (cx fm gi gm gk max-ascender b c index met kern
                 u-scale v-scale x y w h adv left top)
    (once-only (string tex-width tex-height)
      `(with-slots ((,fm face-metrics)
                    (,gi glyph-index)
                    (,gm glyph-metrics)
                    (,gk glyph-kerning))
           ,texatl-font
         (let ((,cx 0.0)
               (,u-scale (/ 1.0 ,tex-width))
               (,v-scale (/ 1.0 ,tex-height))
               (,max-ascender (aref ,fm 0)))
           (loop for ,b = nil then ,c
                 for ,c across ,string
                 as ,index = (gethash ,c ,gi)
                 as ,met = (aref ,gm ,index)
                 as ,kern = (or (aval (cons ,b ,c) ,gk :test 'equal)
                                0.0)
                 do (with-glyph-metrics (,x ,y ,w ,h ,adv ,left ,top) ,met
                      (let* ((,x0 (round (+ ,cx ,left ,kern)))
                             (,y0 (round (- ,max-ascender ,top)))
                             (,x1 (+ ,x0 ,w))
                             (,y1 (+ ,y0 ,h))
                             (,u0 (* ,x ,u-scale))
                             (,v0 (* ,y ,v-scale))
                             (,u1 (+ ,u0 (* ,w ,u-scale)))
                             (,v1 (+ ,v0 (* ,h ,v-scale))))
                        ,@body)
                      (incf ,cx (+ ,adv ,kern)))))))))
