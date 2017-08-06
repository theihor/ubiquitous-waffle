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

;;; copy of connector player

(defclass connector-player-old (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil)
   (current-network :accessor current-network
                    :initform nil)
   (mines :accessor mines
          :initform nil)
   (claimed-mines :accessor claimed-mines
                  :initform nil)))

(defun find-connecting-move-old (graph current-network target)
  (if (gethash target current-network)
      t
      (let ((nodes (alexandria:hash-table-keys current-network))
            (prev (alexandria:copy-hash-table current-network)))
        (labels ((%bfs (nodes)
                   (let ((new-nodes nil))
                     (loop :for node :in nodes
                        :do (mapc-node-edges
                             graph node
                             (lambda (neighbour data)
                               (declare (ignore data))
                               (unless (gethash neighbour prev)
                                 (setf (gethash neighbour prev)
                                       node)
                                 (push neighbour new-nodes)
                                 (when (= neighbour target)
                                   (return-from %bfs t))))))
                     (when new-nodes
                       (%bfs new-nodes))))
                 (%move-from (node prev-node)
                   (let ((node1 (gethash node prev)))
                     (if (eq node1 t)
                         (values node prev-node)
                         (%move-from node1 node)))))
          (if (%bfs nodes)
              (%move-from target (gethash target prev))
              nil)))))

(defmethod make-player ((player-class (eql 'connector-player-old)) &rest params)
  (declare (ignore params))
  (make-instance 'connector-player-old))

(defmethod init-player :after ((player connector-player-old) setup-message)
  (with-slots (avail-graph state current-network) player
    (setf avail-graph (clone-graph (game-map state)))
    (setf current-network (make-hash-table :test #'equal))
    (let* ((mines (mines state))
           (mine-scores
            (loop :for mine :in mines
               :collect
               (cons mine
                     (loop
                        :for other-mine :in mines
                        :for dist = (gethash (cons mine other-mine) (distance-tab state))
                        :when dist
                        :summing dist))))
           (sorted (mapcar #'car
                           (sort (copy-list mine-scores)
                                 #'<
                                 :key #'cdr))))
      (setf (mines player) sorted))))

(defmethod update-player :after ((player connector-player-old) moves)
  (with-slots (current-network avail-graph state mines) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (remove-edge avail-graph src trgt)
       (when (= id (id state))
         (setf (gethash src current-network) t)
         (setf (gethash trgt current-network) t))))))

(defmethod select-move ((player connector-player-old))
  (with-slots (avail-graph current-network mines state claimed-mines) player
    (labels ((%do-move ()
               (if mines
                   (let ((next-mine (car mines)))
                     (multiple-value-bind (src trgt)
                         (find-connecting-move-old avail-graph current-network next-mine)
                       (cond ((or (eq src t)
                                  (eq src nil))
                              (if (eq src t)
                                  (let ((claimed-mine (pop mines)))
                                    (push claimed-mine claimed-mines))
                                  (pop mines))
                              (%do-move))
                             (t (make-claim state src trgt)))))
                   (%do-random-move)))
             (%dist (node)
               (loop
                  :for claimed-mine :in claimed-mines
                  :for val = (gethash (cons claimed-mine node) (distance-tab state))
                  :when val
                  :summing (* val val)))
             (%do-random-move ()
               (let ((max-move nil)
                     (max-dist 0))
                 (maphash (lambda (node val)
                            (declare (ignore val))
                            (let ((neighbour (any-neighbour avail-graph node)))
                              (when (and neighbour
                                         (null (gethash neighbour current-network)))
                                (let ((dist (%dist neighbour)))
                                  (when (> dist max-dist)
                                    (setf max-move (cons node neighbour)))))))
                          current-network)
                 (if max-move
                     (make-claim state (car max-move) (cdr max-move))
                     (%do-totally-random-move))))
             (%do-totally-random-move ()
               (let ((node (any-node avail-graph)))
                 (if node
                     (make-claim state node (any-neighbour avail-graph node))
                     (make-pass state)))))
      (when (= (hash-table-count current-network) 0)
        (let ((first-mine (pop mines)))
          (setf (gethash first-mine current-network) t)
          (push first-mine claimed-mines)))
      (%do-move))))

(defclass future-player (connector-player-old) ())

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
                           (find-connecting-move-old
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
