(uiop:define-package :src/game-protocol
    (:use :common-lisp)
  (:export #:setup
           #:setup-punter
           #:setup-punters
           #:setup-map
           #:setup-settings
           #:game-map
           #:map-sites
           #:map-rivers
           #:map-mines
           #:river
           #:river-source
           #:river-target
           #:move
           #:move-punter
           #:move-state
           #:claim
           #:claim-source
           #:claim-target
           #:pass
           #:splurge
           #:splurge-route
           #:option
           #:stop
           #:stop-moves
           #:stop-scores
           #:score-info
           #:score-info-punter
           #:score-info-score
           #:settings
           #:future
           #:future-source
           #:future-target
           #:settings-options
           #:settings-futures))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-protocol)

;;; handshake
(defclass setup ()
  ((punter :initarg :punter :accessor setup-punter)
   (punters :initarg :punters :accessor setup-punters)
   (setup-map :initarg :map :accessor setup-map)
   (setting :initarg :settings :accessor setup-settings :initform nil)))

(defclass settings ()
  ((futures :initarg :futures :accessor settings-futures :initform nil)
   (splurges :initarg :splurges :accessor settings-splurges :initform nil)
   (options :initarg :options :accessor settings-options :initform nil)))

(defclass future ()
  ((source :initarg :source :accessor future-source)
   (target :initarg :target :accessor future-target)))

(defclass game-map ()
  ((sites :initarg :sites :accessor map-sites)
   (rivers :initarg :rivers :accessor map-rivers)
   (mines :initarg :mines :accessor map-mines)))

(defclass river ()
  ((source :initarg :source :accessor river-source)
   (target :initarg :target :accessor river-target)))

;;; move
(defclass move ()
  ((punter :initarg :punter :accessor move-punter)
   (state :initarg :state :accessor move-state :initform nil)))

(defclass claim (move)
  ((source :initarg :source :accessor claim-source)
   (target :initarg :target :accessor claim-target)))

(defclass pass (move) ())

(defclass splurge (move)
  ((route :initarg :route :accessor splurge-route)))

(defclass option (claim) ())

(defmethod print-object ((c claim) s)
  (format s "<claim ~A->~A>" (claim-source c) (claim-target c)))

(defmethod print-object ((c option) s)
  (format s "<option ~A->~A>" (claim-source c) (claim-target c)))

;;;

(defclass stop ()
  ((moves :initarg :moves :accessor stop-moves)
   (scores :initarg :scores :accessor stop-scores)))

(defclass score-info ()
  ((punter :initarg :punter :accessor score-info-punter)
   (score :initarg :score :accessor score-info-score)))
