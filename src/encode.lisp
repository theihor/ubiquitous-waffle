(uiop:define-package :src/encode
    (:use :common-lisp :src/game-protocol)
  (:export #:encode-me
           #:encode-ready
           #:encode-move))

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

(defmethod yason:encode ((obj pass) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("pass")
        (yason:with-object ()
          (yason:encode-object-element "punter" (move-punter obj)))))))


(defun encode-msg (json-msg)
  (format nil "~a:~a" (length json-msg) json-msg))


(defun encode-me (name)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "me" name))))))

(defun encode-ready (punter-id)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "ready" punter-id))))))

(defun encode-move (move)
  (encode-msg
   (with-output-to-string (stream)
     (yason:encode move stream))))
