(uiop:define-package :src/game-player
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/game-state)
  (:export
   #:make-player
   #:init-player
   #:update-player
   #:select-move
   #:cowboy-player
   #:state))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-player)

(defclass game-player ()
  ((state :accessor state
          :initform nil)))

(defgeneric make-player (player-class &rest params))
(defgeneric init-player (player setup-message))
(defgeneric update-player (player moves))
(defgeneric select-move (player))

(defmethod init-player ((player game-player) setup-message)
  (setf (state player) (make-game-state setup-message)))

(defmethod update-player ((player game-player) moves)
  (process-moves (state player) moves))

(defclass cowboy-player (game-player)
  ((avail-list :accessor avail-list
               :initform nil)
   (avail-tab :accessor avail-tab
              :initform nil)
   (avail-graph :accessor avail-graph
                :initform nil)))

(defmethod make-player ((player-class (eql 'cowboy-player)) &rest params)
  (declare (ignore params))
  (make-instance 'cowboy-player))

(defmethod init-player :after ((player cowboy-player) setup-message)
  (with-slots (avail-list avail-tab avail-graph state) player
    (setf avail-graph (clone-graph (game-map state)))
    (setf avail-tab (make-hash-table :test #'equal))
    (let ((list (mines state)))
      (setf avail-list list)
      (loop :for node :in list
         :do (setf (gethash node avail-tab) t)))))

(defmethod update-player :after ((player cowboy-player) moves)
  (with-slots (avail-list avail-tab avail-graph state) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (declare (ignore id))
       (remove-edge avail-graph src trgt)))))

(defmethod select-move ((player cowboy-player))
  (with-slots (avail-list avail-tab avail-graph state) player
    (labels ((%find-avail (list tab)
               (if (null list)
                   nil
                   (let* ((node (car list))
                          (move-to (any-neighbour avail-graph node)))
                     (if move-to
                         (values node move-to list)
                         (progn
                           (remhash node tab)
                           (%find-avail (cdr list) avail-tab))))))
             (%add-new-avail (node list tab)
               (if (gethash node tab)
                   list
                   (progn
                     (setf (gethash node tab) t)
                     (cons node list)))))
      (multiple-value-bind (node move-to new-avail-list)
          (%find-avail avail-list avail-tab)
        (if node
            (let ((new-avail-list-2 (%add-new-avail move-to new-avail-list avail-tab)))
              (setf avail-list new-avail-list-2)
              (make-instance 'claim
                             :source node
                             :target move-to
                             :punter (id state)))
            (make-instance 'pass :punter (id state)))))))
