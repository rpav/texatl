(in-package :texatl)

(defmacro with-atlas-surface ((surface width height) &body body)
  (with-gensyms (ctx)
    `(let* ((,surface (cairo:create-image-surface :argb32 ,width ,height))
            (,ctx (cairo:create-context ,surface)))
       (cairo:with-context (,ctx)
         (unwind-protect
              (progn ,@body)
           (cairo:destroy ,ctx))))))
