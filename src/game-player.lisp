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
   #:futures))

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
                 :punter (id state)))

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
                          :initform nil)))

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

(defmethod make-player ((player-class (eql 'connector-player)) &rest params)
  (declare (ignore params))
  (make-instance 'connector-player))

(defmethod init-player :after ((player connector-player) setup-message)
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
      (setf (mines player) sorted)
      (setf (starting-locations player)
            (mapcar (lambda (node)
                      (make-location :mine node))
                    sorted)))))

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
