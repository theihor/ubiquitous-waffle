(uiop:define-package :src/future-player
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/game-state
          :src/game-player)
  (:export
   #:future-player
   ))

(in-package :src/future-player)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defclass future-player (connector-player)
  ((futures :initarg :futures :accessor player-futures)))

(defmethod make-player ((player-class (eql 'future-player)) &rest params)
  (declare (ignore params))
  (make-instance 'future-player))

(defun sort-distances (distance-tab)
  "Builds sorted alist of pairs (mine . target)"
  (sort (alexandria:hash-table-alist distance-tab) #'> :key #'cdr))

(defun find-futhest-futures (distance-tab moves)
  "Finds furthest reachable sites. Reachable means there are enough moves to reach them in case other players do not obstruct.
Returns list of (mine . target)"
  (let ((target-list (sort-distances distance-tab))
        (available-moves moves)
        (result-targets))
    (dolist (mine-target target-list (nreverse result-targets))
      (destructuring-bind ((mine . target) . dist) mine-target
        (if (< available-moves dist)
            (return-from
             find-futhest-futures (nreverse result-targets))
            (progn (push
                    (make-instance 'future :source mine :target target)
                    result-targets)
                   (decf available-moves dist)))))))

(defun bid (player setup-map)
  ;; moves = number of rivers
  (let ((state (state player)))
    (find-futhest-futures
     (distance-tab state)
     (truncate (/ (length (map-rivers setup-map))
                  (players-number state))))))

(defmethod init-player :after ((player future-player) setup-message)
  (setf (player-futures player) (bid player (setup-map setup-message))))

(defmethod select-move ((player future-player))
  (with-slots (avail-graph current-network mines state claimed-mines futures)
      player
    (labels ((%do-move ()
               (let ((next-future (car futures))
                     (new-current-network
                      (alexandria:copy-hash-table current-network)))
                 (if next-future
                     (let ((mine (future-source next-future)))
                       (setf (gethash mine new-current-network) t)
                       (multiple-value-bind (src trgt)
                           (find-connecting-move
                            avail-graph new-current-network (future-target next-future))
                         (cond
                           ((or (eq src t)
                                (eq src nil))
                            (pop futures)
                            (%do-move))
                           (t (when (or (eq src mine)
                                        (eq trgt mine))
                                (pushnew mine claimed-mines))
                              (make-claim state src trgt)))))
                     (call-next-method)))))
      (%do-move))))

(defmethod bid-on-futures ((player future-player) setup-message)
  (declare (ignore setup-message))
  (player-futures player))
