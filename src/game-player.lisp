(uiop:define-package :src/game-player
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/game-state)
  (:import-from :alexandria)
  (:export
   #:make-player
   #:init-player
   #:update-player
   #:select-move
   #:game-player
   #:cowboy-player
   #:connector-player
   #:state
   #:make-pass
   #:make-claim
   #:game-player
   #:find-connecting-move
   #:make-claim
   #:avail-graph
   #:current-network
   #:mines
   #:claimed-mines
   #:player-futures
   #:futures
   #:find-regions-connecting-move))

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(in-package :src/game-player)

(defclass game-player ()
  ((state :accessor state
          :initarg :state
          :initform nil)
   (futures :initarg :futures 
            :accessor player-futures)))

(defgeneric make-player (player-class &rest params))
(defgeneric init-player (player setup-message))
(defgeneric update-player (player moves))
(defgeneric select-move (player))

(defmethod init-player ((player game-player) setup-message)
  (setf (state player) (make-game-state setup-message))
  (setf (player-futures player) nil))

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

(defun make-claim (state src trgt)
  (make-instance 'claim
                 :source src
                 :target trgt
                 :punter (id state))
  ;; (make-instance 'splurge
  ;;                :punter (id state)
  ;;                :route (list src trgt))
  )

(defun make-pass (state)
  (make-instance 'pass :punter (id state)))

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
              (make-claim state node move-to))
            (make-pass state))))))

;; Connector player

(defclass location ()
  ((kind :initarg :kind
         :accessor kind)
   (node :initarg :node
         :accessor node)
   (data :initarg :data
         :accessor data
         :initform nil)))

(defun make-location (kind node &optional data)
  (make-instance 'location
                 :kind kind
                 :node node
                 :data data))

(defun mine-location? (location)
  (eq (kind location) :mine))

(defclass connector-player (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil)
   (current-network :accessor current-network
                    :initform nil)
   ;; TODO: remove when future-player is fixed
   (mines :accessor mines
          :initform nil)
   (claimed-mines :accessor claimed-mines
                  :initform nil)
   (locations :accessor locations
              :initform nil)
   (starting-locations :accessor starting-locations
                       :initform nil)
   (not-reached-locations :accessor not-reached-locations
                          :initform nil)
   (gambling :accessor gambling
            :initform nil
            :initarg :gambling)))

(defun find-connecting-move (graph current-network target)
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

(defun hash-tables-intersect? (big-tab small-tab)
  (maphash (lambda (key val)
             (declare (ignore val))
             (when (gethash key big-tab)
               (return-from hash-tables-intersect? key)))
           small-tab)
  nil)

(defun hash-table-add (big-tab small-tab)
  (maphash (lambda (key val)
             (setf (gethash key big-tab) val))
           small-tab))

(defun cluster-freedom (graph cluster)
  (let ((adj (make-hash-table :test #'equal)))
    (maphash (lambda (node val)
               (declare (ignore val))
               (mapc-node-edges graph node
                                (lambda (neighbour data)
                                  (declare (ignore data))
                                  (setf (gethash neighbour adj) t))))
             cluster)
    (hash-table-count adj)))

(defun singleton-hash-table (value)
  (let ((tab (make-hash-table :test #'equal)))
    (setf (gethash value tab) t)
    tab))

(defun find-regions-connecting-move (graph current-network target-cluster)
  (let ((found (hash-tables-intersect? current-network target-cluster)))
    (if found
        (list found)
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
                                   (when (gethash neighbour target-cluster)
                                     (return-from %bfs (values t neighbour)))))))
                       (when new-nodes
                         (%bfs new-nodes))))
                   (%move-from (node acc)
                     (let ((node1 (gethash node prev)))
                       (if (eq node1 t)
                           acc
                           (%move-from node1 (cons node acc))))))
            (multiple-value-bind (path-found target)
                (%bfs nodes)
              (if path-found
                  (%move-from target nil)
                  nil)))))))

(defun estimate-node-score (node claimed-mines distance-tab)
  (loop
     :for claimed-mine :in claimed-mines
     :for val = (gethash (cons claimed-mine node) distance-tab)
     :when val
     :summing (* val val)))

(defun node-with-max-distance (mine distance-tab node->moves moves-cap)
  (let ((result-node nil)
        (max-dist 0))
    (maphash (lambda (node move-num)
               (when (< move-num moves-cap)
                 (let ((dist (gethash (cons mine node) distance-tab))
                       (rev-dist (gethash (cons node mine) distance-tab)))
                   ;; Do not bid on mines
                   (when (and (null rev-dist)
                              (> dist max-dist))
                     (setf max-dist dist)
                     (setf result-node node)))))
             node->moves)
    (values result-node max-dist)))

(defun build-full-network (graph distance-tab start-mine mines num-moves
                           &key gamble)
  (let ((current-network (make-hash-table :test #'equal))
        (move-num-tab (make-hash-table :test #'equal))
        (mine-tab (make-hash-table :test #'equal))
        (target-order nil)
        (claimed-mines nil))
    (loop :for mine :in mines
       :do (setf (gethash mine mine-tab) t))
    ;; (push start-mine claimed-mines)
    ;; (push start-mine target-order)
    (setf (gethash start-mine current-network) t)
    (setf (gethash start-mine move-num-tab) 0)
    (labels ((%add-target (target)
               (setf (gethash target current-network) t)
               (push target target-order)
               (when (gethash target mine-tab)
                 (push target claimed-mines)
                 (remhash target mine-tab)))
             (%add-intermediate (node len)
               (setf (gethash node current-network) t)
               (setf (gethash node move-num-tab) len))
             (%iter (move-num)
               (let ((path (find-regions-connecting-move graph current-network mine-tab)))
                 (cond ((null path)
                        (%iter-non-targeted move-num))
                       ((null (cdr path))
                        (%add-target (car path))
                        (%iter move-num))
                       (t
                        (let ((path-nodes (cdr path)))
                          (loop :for node :in path-nodes
                             :do (progn
                                   (when (>= move-num num-moves)
                                     (return-from %iter))
                                   (%add-intermediate node move-num)
                                   (incf move-num)))
                          (%add-target (car (last path))))
                        (%iter move-num)))))
             (%iter-non-targeted (move-num)
               (when (< move-num num-moves)
                 (let ((move (find-best-non-targeted-move graph current-network distance-tab claimed-mines)))
                   ;; (format t "Best non-targeted ~A~%" move)
                   (when move
                     (%add-intermediate (cdr move) move-num)
                     (%iter-non-targeted (1+ move-num))))))
             (%network-score ()
               (let ((score 0))
                 (maphash (lambda (node val)
                            (declare (ignore val))
                            (incf score (estimate-node-score node claimed-mines distance-tab)))
                          current-network)
                 ;; (format t "Network : ~A~%" (alexandria:hash-table-keys current-network))
                 ;; (format t "Computed score for order ~A, claimed ~A, score: ~A~%"
                 ;;         (reverse target-order) claimed-mines score)
                score))
             (%moves-cap ()
               (floor (* 0.5 num-moves)))
             (%gamble ()
               (let ((futures nil))
                 (loop :for mine :in claimed-mines
                    :do (multiple-value-bind (node dist)
                            (node-with-max-distance mine distance-tab
                                                    move-num-tab (%moves-cap))
                          (when node
                            (push (list mine node (* dist dist dist))
                                  futures))))
                 ;; (format t "Futures : ~A~%" futures)
                 futures))
             (%sort-by-num-move (targets)
               (stable-sort (copy-list targets)
                            #'<
                            :key (lambda (node)
                                   (or (gethash node move-num-tab)
                                       1000000)))))
      (%iter 0)
      (let ((mines-order (append
                          (reverse target-order)
                          ;; Add other mines
                          (alexandria:hash-table-keys mine-tab)))
            (score (%network-score)))
        (if gamble
            (let ((futures (%gamble)))
              (values (%sort-by-num-move (append mines-order
                                                 (remove-duplicates (mapcar #'second futures) :test #'equal)))
                      (+ score (reduce #'+ (mapcar #'third futures)))
                      futures))
            (values mines-order score))))))

(defun find-best-mines-order (graph distance-tab mines num-moves &key gamble)
  (let ((max-score 0)
        (best-order nil)
        (best-futures nil))
    (loop :for mine :in mines
       :do (multiple-value-bind (order score futures)
               (build-full-network graph distance-tab mine mines num-moves
                                   :gamble gamble)
             (when (> score max-score)
               (setf max-score score)
               (setf best-order order)
               (setf best-futures futures))))
    ;; (format t "Best order : ~A~%" best-order)
    (values (or best-order
                mines)
            best-futures)))

(defun find-best-non-targeted-move (graph current-network distance-tab claimed-mines)
  (let ((max-move nil)
        (max-dist 0))
    (maphash (lambda (node val)
               (declare (ignore val))
               (mapc-node-edges
                graph node
                (lambda (neighbour data)
                  (declare (ignore data))
                  (when (and neighbour
                             (null (gethash neighbour current-network)))
                    (let ((dist (estimate-node-score neighbour claimed-mines distance-tab)))
                      (when (> dist max-dist)
                        (setf max-move (cons node neighbour))))))))
             current-network)
    max-move))

(defmethod make-player ((player-class (eql 'connector-player)) &rest params &key gambling)
  (declare (ignore params))
  (make-instance 'connector-player
                 :gambling gambling))

(defmethod init-player :after ((player connector-player) setup-message)
  (with-slots (avail-graph state current-network gambling futures) player
    (setf avail-graph (clone-graph (game-map state)))
    (setf current-network (make-hash-table :test #'equal))
    (let* ((mines (mines state))
           ;; (mine-scores
           ;;  (loop :for mine :in mines
           ;;     :collect
           ;;     (cons mine
           ;;           (loop
           ;;              :for other-mine :in mines
           ;;              :for dist = (gethash (cons mine other-mine) (distance-tab state))
           ;;              :when dist
           ;;              :summing dist))))
           ;; (sorted (mapcar #'car
           ;;                 (sort (copy-list mine-scores)
           ;;                       #'<
           ;;                       :key #'cdr)))
           )
      (multiple-value-bind (sorted cool-futures)
          (find-best-mines-order avail-graph (distance-tab state)
                                 mines (floor (length (map-rivers (setup-map setup-message)))
                                              (players-number state))
                                 :gamble gambling)
        (setf (mines player) sorted)
        (when cool-futures
          (setf futures
                (loop :for (mine node _) :in cool-futures
                   :collect (make-instance 'future
                                           :source mine
                                           :target node))))
        (setf (starting-locations player)
              (mapcar (lambda (node)
                        (make-location :mine node))
                      sorted))))))

(defmethod update-player :after ((player connector-player) moves)
  (with-slots (current-network avail-graph state) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (remove-edge avail-graph src trgt)
       (when (= id (id state))
         (setf (gethash src current-network) t)
         (setf (gethash trgt current-network) t))))))

(defun extract-first (list predicate &optional acc)
  (if (null list)
      (values nil (reverse acc))
      (if (funcall predicate (car list))
          (values (car list) (append (reverse acc) (cdr list)))
          (extract-first (cdr list) predicate (cons (car list) acc)))))

(defmethod select-move ((player connector-player))
  (with-slots (avail-graph current-network locations
                           starting-locations state claimed-mines
                           not-reached-locations)
      player
    (labels ((%claim (location)
               (when (mine-location? location)
                 (push (node location) claimed-mines)))
             (%do-move ()
               (if locations
                   (let ((next-location (car locations)))
                     (multiple-value-bind (src trgt)
                         (find-connecting-move avail-graph current-network (node next-location))
                       (cond ((or (eq src t)
                                  (eq src nil))
                              (let ((claimed-location (pop locations)))
                                (if (eq src t)
                                    (%claim claimed-location)
                                    (push claimed-location not-reached-locations)))
                              (%do-move))
                             (t (make-claim state src trgt)))))
                   (%do-random-move)))
             (%do-random-move ()
               (let ((max-move (find-best-non-targeted-move avail-graph current-network
                                                            (distance-tab state) claimed-mines)))
                 (if max-move
                     (make-claim state (car max-move) (cdr max-move))
                     (%try-not-reached))))
             (%try-not-reached ()
               (if not-reached-locations
                   (progn
                     (%init-locations (reverse not-reached-locations))
                     (setf not-reached-locations nil)
                     (%do-move))
                   (%do-totally-random-move)))
             (%do-totally-random-move ()
               (let ((node (any-node avail-graph)))
                 (if node
                     (make-claim state node (any-neighbour avail-graph node))
                     (make-pass state))))
             (%filter-locs (locs)
               (remove-if-not
                (lambda (loc)
                  (any-neighbour avail-graph (node loc)))
                locs))
             (%init-locations (locs)
               (let ((alive-locs (%filter-locs locs)))
                 (multiple-value-bind (first-loc other-locs)
                     (extract-first alive-locs #'mine-location?)
                   (if first-loc
                       (progn
                         (setf (gethash (node first-loc) current-network) t)
                         (%claim first-loc)
                         (setf locations other-locs))
                       ;; Oops
                       (setf locations nil))))))
      (when (= (hash-table-count current-network) 0)
        (%init-locations starting-locations))
      (%do-move))))
