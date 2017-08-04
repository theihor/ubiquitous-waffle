(uiop:define-package :src/game-protocol
    (:use :common-lisp)
  (:export #:setup
           #:setup-punter
           #:setup-punters
           #:setup-map
           #:game-map
           #:map-sites
           #:map-rivers
           #:map-mines
           #:river
           #:river-source
           #:river-target
           #:move
           #:move-punter
           #:claim
           #:claim-source
           #:claim-target
           #:pass
           #:stop
           #:stop-moves
           #:stop-scores
           #:score-info
           #:score-info-punter
           #:score-info-score
           ))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-protocol)

;;; handshake
(defclass setup ()
  ((punter :initarg :punter :accessor setup-punter)
   (punters :initarg :punters :accessor setup-punters)
   (setup-map :initarg :map :accessor setup-map)))

(defclass game-map ()
  ((sites :initarg :sites :accessor map-sites)
   (rivers :initarg :rivers :accessor map-rivers)
   (mines :initarg :mines :accessor map-mines)))

(defclass river ()
  ((source :initarg :source :accessor river-source)
   (target :initarg :target :accessor river-target)))

;;; move
(defclass move ()
  ((punter :initarg :punter :accessor move-punter)))

(defclass claim (move)
  ((source :initarg :source :accessor claim-source)
   (target :initarg :target :accessor claim-target)))

(defmethod print-object ((c claim) s)
  (format s "~A->~A" (claim-source c) (claim-target c)))

(defclass pass (move) ())
;;;

(defclass stop ()
  ((moves :initarg :moves :accessor stop-moves)
   (scores :initarg :scores :accessor stop-scores)))

(defclass score-info ()
  ((punter :initarg :punter :accessor score-info-punter)
   (score :initarg :score :accessor score-info-score)))
