(uiop:define-package :src/encode
    (:use :common-lisp :anaphora :src/game-protocol)
  (:export #:encode-me
           #:encode-you
           #:encode-setup
           #:encode-ready
           #:encode-move
           #:encode-moves
           #:encode-stop
           #:*yason-lisp-readable-encode*))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/encode)

(defun encode-claim (obj)
  (yason:encode-object-element "punter" (move-punter obj))
  (yason:encode-object-element "source" (claim-source obj))
  (yason:encode-object-element "target" (claim-target obj)))

(defmethod yason:encode ((obj claim) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("claim")
        (yason:with-object () (encode-claim obj)))
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

(defmethod yason:encode ((obj splurge) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("splurge")
        (yason:with-object ()
          (yason:encode-object-element "punter" (move-punter obj))
          (yason:encode-object-element "route" (splurge-route obj))))
      (awhen (move-state obj)
        (yason:encode-object-element "state" it)))))

(defmethod yason:encode ((obj option) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("option")
        (yason:with-object ()
          (encode-claim obj)))
      (awhen (move-state obj)
        (yason:encode-object-element "state" it)))))

(defmethod yason:encode ((obj game-map) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("sites")
        (yason:with-array ()
          (loop :for site :in (map-sites obj) :do
             (yason:with-object ()
               (yason:encode-object-element "id" site))))) 
      (yason:encode-object-element "rivers" (map-rivers obj))
      (yason:encode-object-element "mines" (map-mines obj)))))

(defmethod yason:encode ((obj river) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "source" (river-source obj))
      (yason:encode-object-element "target" (river-target obj)))))

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

(defun encode-you (name)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "you" name))))))

(defun encode-setup (setup)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "punter" (setup-punter setup))
         (yason:encode-object-element "punters" (setup-punters setup))
         (yason:encode-object-element "map" (setup-map setup))
         (yason:encode-object-element "settings" (setup-settings setup)))))))

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

(defun encode-moves (moves state)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "moves" moves)
         (yason:encode-object-element "state" state))))))

(defun encode-stop (moves scores)
  (encode-msg
   (with-output-to-string (stream)
     (yason:with-output (stream)
       (yason:with-object ()
         (yason:encode-object-element "moves" moves)
         (yason:encode-object-element "scores" scores))))))

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

(defparameter *yason-lisp-readable-encode* t)

(defmethod yason:encode :around ((obj hash-table) &optional stream)
  (if *yason-lisp-readable-encode*
      (yason:with-output (stream)
        (yason:with-object ()
          (yason:encode-object-element
           "__type" "HASH-TABLE")
          (yason:encode-object-element
           "content" (alexandria:hash-table-plist obj))))
      (call-next-method)))

(defmethod yason:encode ((obj symbol) &optional stream)
  (yason:with-output (stream)
    (let ((prefix (if (keywordp obj) ":" "#")))
      (yason:encode (format nil "~A~A" prefix obj) stream))))


(defmethod yason:encode ((obj score-info) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "punter" (score-info-punter obj))
      (yason:encode-object-element "scores" (score-info-score obj)))))
