;;; There should be no foreign (i.e. non-CL) dependencies in this file.
(in-package :texatl.cl)

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

(defun texatl-string-width (string font)
  "=> PIXEL-WIDTH

Return the width of `STRING` when rendered with `FONT`.  This is the
width from the placement of the first glyph to the position past the
final glyph."
  (with-slots (face-metrics glyph-index glyph-metrics glyph-kerning)
      font
    (let ((cx 0.0))
      (loop for b = nil then c
            for c across string
            as index = (gethash c glyph-index)
            as met = (aref glyph-metrics index)
            as kerning = (or (cdr (assoc (cons b c) glyph-kerning
                                         :test 'equal))
                             0.0)
            do (with-glyph-metrics (x y w h adv left top) met
                 (declare (ignore x y w h left top))
                 (incf cx (+ adv kerning))))
      cx)))
