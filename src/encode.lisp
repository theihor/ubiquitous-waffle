(uiop:define-package :src/encode
    (:use :common-lisp :anaphora :src/game-protocol)
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
          (yason:encode-object-element "target" (claim-target obj))))
      (awhen (move-state obj)
        (yason:encode-object-element "state" it)))))

(defmethod yason:encode ((obj pass) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("pass")
        (yason:with-object ()
          (yason:encode-object-element "punter" (move-punter obj))))
      (awhen (move-state obj)
        (yason:encode-object-element "state" it)))))

(defmethod yason:encode ((obj future) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "source" (future-source obj))
      (yason:encode-object-element "target" (future-target obj)))))

(defun encode-msg (json-msg)
  (format nil "~a:~a" (length json-msg) json-msg))


(defun encode-me (name)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "me" name))))))

(defun encode-ready (punter-id &key futures state)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "ready" punter-id)
         (when futures
           (yason:encode-object-element "futures" futures))
         (when state
           (yason:encode-object-element "state" state)))))))

(defun encode-move (move)
  (encode-msg
   (with-output-to-string (stream)
     (yason:encode move stream))))

;;;
;;; universal encoder
;;;
(defmethod yason:encode ((obj standard-object) &optional stream)
  (let ((class (class-of obj)))
    (yason:with-output (stream)
      (yason:with-object ()
        (yason:encode-object-element
         "__type" (string-upcase (string (class-name class))))
        (dolist (slot (sb-mop:class-slots class))
          (let ((slot-name (sb-mop:slot-definition-name slot)))
            (yason:encode-object-element
             (string slot-name) (slot-value obj slot-name))))))))

(defmethod yason:encode ((obj hash-table) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element
       "__type" "HASH-TABLE")
      (yason:encode-object-element
       "content" (alexandria:hash-table-plist obj)))))

(defmethod yason:encode ((obj cons) &optional stream)
  (yason:with-output (stream)
    (if (listp (cdr obj))
        (yason:with-array ()
          (dolist (x obj)
            (yason:encode-array-element x)))
        (yason:with-object ()
          (yason:encode-object-element
           "__type" "PAIR")
          (yason:encode-object-element
           "car" (car obj))
          (yason:encode-object-element
           "cdr" (cdr obj))))))

(defmethod yason:encode ((obj symbol) &optional stream)
  (yason:with-output (stream)
    (let ((prefix (if (keywordp obj) ":" "#")))
      (yason:encode (format nil "~A~A" prefix obj) stream))))

