(in-package :texatl)

(defun place-sprite (sprite x y)
  (let ((w (cairo:width sprite))
        (h (cairo:height sprite)))
    (cairo:set-source-surface sprite x y)
    (cairo:rectangle x y w h)
    (cairo:fill-path)))

(defun make-sprite-id (filename)
  (multiple-value-bind (frame frame-p)
      (ppcre:regex-replace ".*:(\\d+)$" (pathname-name filename) "\\1")
    (let ((name (ppcre:regex-replace "(.*):\\d+$" (pathname-name filename)
                                     "\\1")))
      (values
       (mapcar (lambda (x)
                 (make-keyword (string-upcase x)))
               (ppcre:split ":" name))
       (if frame-p (parse-integer frame) 0)))))

(defun add-sprite-metrics (sheet name frame x y w h)
  (with-slots ((metrics texatl.cl::metrics)) sheet
    (let ((framevec (or (gethash name metrics)
                        (make-array 1 :adjustable t))))
      (when (<= (length framevec) frame)
        (adjust-array framevec (1+ frame)))
      (setf (aref framevec frame)
            (make-array 4 :element-type 'single-float
                          :initial-contents
                          (mapcar #'float
                                  (list x y
                                        (+ x w) (+ y h)))))
      (setf (gethash name metrics) framevec))))

(defun make-sprite-atlas (width height file-list)
  (with-atlas-surface (surface width height)
    (let ((cx 0.0)
          (cy 0.0)
          (tallest 0.0)
          (metrics (make-instance 'texatl.cl:texatl-spritesheet)))
      (loop for filename in file-list
            for sprite = (cairo:image-surface-create-from-png (namestring filename))
            as w = (cairo:width sprite)
            as h = (cairo:height sprite)
            do (multiple-value-bind (sprite-id frame)
                   (make-sprite-id filename)
                 (when (> (+ cx w) width)
                   (incf cy tallest)
                   (setf tallest 0.0
                         cx 0.0))
                 (setf tallest (max tallest h))
                 (add-sprite-metrics metrics sprite-id frame cx cy w h)
                 (place-sprite sprite cx cy)
                 (incf cx w)
                 (cairo:destroy sprite)))
      (values surface metrics))))

(defun make-sprite-atlas-files (png-filename metrics-filename width height
                                file-list)
  (multiple-value-bind (surface metrics)
      (make-sprite-atlas width height file-list)
    (cairo:surface-write-to-png surface (namestring (merge-pathnames png-filename)))
    (cairo:destroy surface)
    (with-open-file (stream (merge-pathnames metrics-filename)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (conspack:encode metrics :stream stream)))
  (values))

