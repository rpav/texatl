(in-package :texatl.cl)

(defclass texatl-spritesheet ()
  ((metrics :initform (make-hash-table :test 'equal) :initarg :metrics)))

(defmethod conspack:encode-object ((object texatl-spritesheet)
                                   &key &allow-other-keys)
  (with-slots (metrics) object
    (alist :metrics metrics)))

(defmethod conspack:decode-object ((class (eql 'texatl-spritesheet))
                                   alist &key &allow-other-keys)
  (alist-bind (metrics) alist
    (make-instance 'texatl-spritesheet :metrics metrics)))

(defun sprite (spritesheet name frame)
  "Return a float-vector in the form #(X0 Y0 X1 Y1) for a sprite given
`NAME` and `FRAME`."
  (with-slots (metrics) spritesheet
    (aref (gethash name metrics) frame)))

(defun frame-count (spritesheet name)
  "Return the number of frames for sprite named `NAME` in spritesheet."
  (with-slots (metrics) spritesheet
    (length (gethash name metrics))))
