(in-package :src/main)

;;; handshake
(defclass setup ()
  ((punter :initarg :punter :accessor setup-punter)
   (punters :initarg :punters :accessor setup-punters)
   (map :initarg :map :accessor setup-map)))

(defclass map ()
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

(defclass pass (move) ())
;;;

(defclass stop ()
  ((moves :initarg :moves :accessor stop-moves)
   (scores :initarg :scores :accessor stop-scores)))

