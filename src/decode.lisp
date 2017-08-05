(uiop:define-package :src/decode
    (:use :common-lisp :anaphora :src/game-protocol)
  (:export #:parse
           #:parse-you
           #:parse-setup
           #:parse-moves
           #:parse-stop
           #:parse-map
           #:total-parse))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/decode)

(defun parse-rivers-inner (rivers-lst)
  (mapcar
   (lambda (river-ht)
     (make-instance
      'river
      :source (gethash "source" river-ht)
      :target (gethash "target" river-ht)))
   rivers-lst))

(defun parse-sites-inner (sites-lst)
  (mapcar (lambda (x) (gethash "id" x)) sites-lst))

(defun parse-map-inner (map-ht)
  (make-instance
   'game-map
   :sites (parse-sites-inner (gethash "sites" map-ht))
   :rivers (parse-rivers-inner (gethash "rivers" map-ht))
   :mines (gethash "mines" map-ht)))

(defun parse-claim (claim-ht)
  (make-instance
   'claim
   :punter (gethash "punter" claim-ht)
   :source (gethash "source" claim-ht)
   :target (gethash "target" claim-ht)))

(defun parse-pass (pass-ht)
  (make-instance
   'pass
   :punter (gethash "punter" pass-ht)))

(defun parse-move (move-ht)
  (acond
    ((gethash "claim" move-ht)
     (parse-claim it))
    ((gethash "pass" move-ht)
     (parse-pass it))
    (t (error "unknown type of move"))))

(defun parse-moves-inner (moves-ht)
  (mapcar #'parse-move (gethash "moves" moves-ht)))

(defun parse-score (score-ht)
  (make-instance
   'score-info
   :punter (gethash "punter" score-ht)
   :score (gethash "score" score-ht)))

(defun parse-stop-inner (stop-ht)
  (make-instance
   'stop
   :moves (mapcar #'parse-move (gethash "moves" stop-ht))
   :scores (mapcar #'parse-score (gethash "scores" stop-ht))))


(defun get-handshake (msg-ht)
  (gethash "you" msg-ht))

(defun setup-p (msg-ht)
  (and (gethash "punter" msg-ht)
       (gethash "punters" msg-ht)
       (gethash "map" msg-ht)))

(defun parse-settings-inner (settings-ht)
  (make-instance
   'settings
   :futures (when settings-ht
              (gethash "futures" settings-ht))))

(defun parse-setup-inner (setup-ht)
  (make-instance
   'setup
   :punter (gethash "punter" setup-ht)
   :punters (gethash "punters" setup-ht)
   :map (parse-map-inner (gethash "map" setup-ht))
   :settings (parse-settings-inner (gethash "settings" setup-ht))))

(defun get-move (msg-ht)
  (gethash "move" msg-ht))

(defun get-stop (msg-ht)
  (gethash "stop" msg-ht))

(defun get-timeout (msg-ht)
  (gethash "timeout" msg-ht))

(defun parse-state (msg-ht)
  msg-ht)

;; (defun total-parse-inner (ht)
;;   (flet ((%slot-name (slot)
;;            (first slot)))
;;    (aif (gethash "__type" ht)
;;         (let ((instance (make-instance (intern it))))
;;           (dolist (slot  instance)
;;             (setf
;;              (intern (string-upcase (cl-mop:slot-name instance (%slot-name slot))))
;;              (gethash (%slot-name slot) ht))))
;;         ht)))


(defun parse-you (msg)
  (let ((you-ht (yason:parse msg)))
    (get-handshake you-ht)))

(defun parse-setup (msg)
  (let ((setup-ht (yason:parse msg)))
    (parse-setup-inner setup-ht)))

(defun parse-moves (msg)
  (let ((move-ht (yason:parse msg)))
    (parse-moves-inner (get-move move-ht))))

(defun parse-stop (msg)
  (let ((score-ht (yason:parse msg)))
    (parse-stop-inner (get-stop score-ht))))

(defun parse-map (map)
  (parse-map-inner (yason:parse map)))

(defun parse (json)
  (let ((json-ht (yason:parse json)))
    (values
     (acond
       ((get-handshake json-ht)
        it)
       ((setup-p json-ht)
        (parse-setup-inner json-ht))
       ((get-move json-ht)
        (parse-moves-inner it))
       ((get-stop json-ht)
        (parse-stop-inner it))
       ((get-timeout json-ht)
        it))
     (parse-state (gethash "state" json-ht)))))

(defun parse-map-from-file (map-file) 
  (with-open-file (stream map-file)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      (parse-map contents))))

;; (defun total-parse (json)
;;   (let ((json-ht (yason:parse json)))
;;     (total-parse-inner json-ht)))
