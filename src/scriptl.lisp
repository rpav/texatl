(in-package :texatl)

(defun scriptl-error (e)
  (format t "~A~%" e)
  t)

(defun make-texatl-script ()
  (scriptl:make-script "texatl" 'texatl-cmd 'scriptl-error "texatl-scriptl"))

(scriptl:register 'texatl 'make-texatl-script "Texatl commands: texatl")

(defun texatl-cmd (&rest args)
  (unix-options:with-cli-options
   (args "~
Usage: texatl [options] SPRITES...
       texatl -S sheet-name

Options:
~@{~A~%~}~%")
      ((output ((#\o "output") "PREFIX" "File prefix (no extension) for output"))
       (width ((#\w "width") "WIDTH" "Width of sprite sheet"))
       (height ((#\h "height") "HEIGHT" "Height of sprite sheet"))
       (split ((#\S "split") nil "Split the sheet, instead of creating it"))
       unix-options:&free free)
    (cond
      (split
       (let ((met-file (string+ (car free) ".met"))
             (png-file (string+ (car free) ".png")))
         (split-sprite-atlas-file png-file met-file)))
      (t
       (let* ((output (or output "sprites"))
              (width (or width 128))
              (height (or height 128))
              (met-file (string+ output ".met"))
              (png-file (string+ output ".png")))
         (make-sprite-atlas-files png-file met-file width height free))))))
