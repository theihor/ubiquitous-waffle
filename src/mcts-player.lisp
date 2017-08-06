(uiop:define-package :src/mcts-player
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/game-state
          :src/game-player
          :src/mcts
          :src/utils
          :src/punter
          )
  (:export #:mcts-player))

(in-package :src/mcts-player)

(defparameter *max-estimation-depth* 100)

(defclass mcts-player (game-player)
  ((player->edges :initform (make-hash-table)
                  :reader player->edges
                  :initarg :player->edges)))

(defmethod init-player ((player mcts-player) setup-message)
  (setf (state player)
        (make-game-state setup-message :type 'game-with-scores)))

(defun clone-mcts-player (player)
  (copy-instance
   player
   :state (clone-game (state player))
   :player->edges (alexandria:copy-hash-table
                   (player->edges player)
                   :key #'copy-list)))

;; player methods

(defmethod make-player ((_ (eql 'mcts-player)) &rest params)
  (declare (ignore params))
  (make-instance 'mcts-player))

(defmethod update-player :after ((p mcts-player) moves)
  (mapc-claims
   moves
   (lambda (id from to)
     (push (cons from to) (gethash id (player->edges p))))))

(defmethod select-move ((p mcts-player))
  ;; todo: set this according to the size of the map
  (let ((*max-estimation-depth* 500))
    (or (let ((tuple (select-next-move :root-state p
                                       :root-player (id (state p))
                                       :max-iters 1000
                                       :max-selection-depth 500
                                       :players-number (players-number (state p)))))
          (when tuple
            (destructuring-bind (from to id) tuple
              (make-instance 'claim
                             :source from
                             :target to
                             :punter id))))
        (make-pass (state p)))))

;; mcts methods

(defmethod possible-actions ((p mcts-player) player-id)
  ;; all possible claims from mines or any edges owned by player
  (let ((actions (make-hash-table :test #'equal))
        (g (game-map (state p))))
    (labels ((%add (from)
               (mapc-node-edges
                g from
                (lambda (to graph-data)
                  (when (eq graph-data :free)
                    (multiple-value-bind (from to) (if (> from to)
                                                       (values to from)
                                                       (values from to))
                      (setf (gethash (list from to player-id) actions)
                            t)))))))
      (loop for (from . to) in (gethash player-id (player->edges p))
         do
           (%add from)
           (%add to))
      (mapc #'%add (mines (state p))))
    (alexandria:hash-table-keys actions)))

(defmethod next-state ((p mcts-player) move)
  (destructuring-bind (from to player-id) move
    (update-player p (list
                      (make-instance 'claim
                                     :source from
                                     :target to
                                     :punter player-id))))
  p)

(defmethod clone-state ((p mcts-player))
  (clone-mcts-player p))

(defmethod estimate-state-rewards ((initial mcts-player) initial-player-id)
  (let ((p initial)
        (player-id initial-player-id)
        (players-number (players-number (state initial))))
    (loop for i below *max-estimation-depth* do
         (let* ((actions-list (possible-actions p player-id)))
           (unless actions-list
             (return))
           (let* ((actions (coerce actions-list 'vector))
                  (n (random (array-dimension actions 0))))
             (setf p (next-state p (aref actions n)))
             (setf player-id (mod (1+ player-id) players-number)))))
    (make-array players-number
                :initial-contents (loop for i below (players-number (state p)) collect
                                       (score (elt (punters (state p)) i))))))
