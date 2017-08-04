(uiop:define-package :src/encode
    (:use :common-lisp :src/game-protocol)
  (:export #:encode-me))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/encode)

(defmethod yason:encode ((obj claim) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("claim")
        (yason:with-object ()
          (yason:encode-object-element "punter" (move-punter obj))
          (yason:encode-object-element "source" (claim-source obj))
          (yason:encode-object-element "target" (claim-target obj)))))))

(defun encode-me (name)
  (let ((result-str
         (with-output-to-string (stream)
           (yason:with-output (stream)
             (yason:with-object ()
               (yason:encode-object-element "me" name))))))
    (format nil "~a:~a" (length result-str) result-str)))
