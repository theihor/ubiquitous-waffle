(uiop:define-package :src/decode
    (:use :common-lisp :anaphora :src/game-protocol)
  (:export #:parse-you
           #:parse-setup
           #:parse-moves
           #:parse-stop
           #:parse-map))

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


(defun parse-you (msg)
  (let ((you-ht (yason:parse msg)))
    (gethash "you" you-ht)))

(defun parse-setup (msg)
  (let ((setup-ht (yason:parse msg)))
    (make-instance
     'setup
     :punter (gethash "punter" setup-ht)
     :punters (gethash "punters" setup-ht)
     :map (parse-map-inner (gethash "map" setup-ht)))))

(defun parse-moves (msg)
  (let ((move-ht (yason:parse msg)))
    (parse-moves-inner (gethash "move" move-ht))))

(defun parse-stop (msg)
  (let ((score-ht (yason:parse msg)))
    (parse-stop-inner (gethash "stop" score-ht))))

(defun parse-map (map)
  (parse-map-inner (yason:parse map)))
