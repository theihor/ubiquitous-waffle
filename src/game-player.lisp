(uiop:define-package :src/game-player
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/game-state
          :src/k-shortest)
  (:import-from :alexandria)
  (:export
   #:make-player
   #:init-player
   #:update-player
   #:select-move
   #:get-player-name
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
   #:find-regions-connecting-move
   #:location))

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(in-package :src/game-player)

(defclass game-player ()
  ((state :accessor state
          :initarg :state
          :initform nil)
   (futures :initarg :futures 
            :accessor player-futures)
   (verbose :initarg :verbose
            :accessor verbose
            :initform nil)))

(defmacro player-log (player str &rest params)
  `(when (verbose ,player)
     (format *error-output* ,str ,@params)))

(defgeneric make-player (player-class &rest params))
(defgeneric init-player (player setup-message))
(defgeneric update-player (player moves))
(defgeneric select-move (player))
(defgeneric get-player-name (player))

(defmethod get-player-name (player)
  (symbol-name (class-name (class-of player))))

(defmethod init-player ((player game-player) setup-message)
  (setf (state player) (make-game-state setup-message))
  (setf (player-futures player) nil))

(defmethod update-player ((player game-player) moves)
  ;; (process-moves (state player) moves)
  )

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

(defun make-claim-player (player src trgt)
  (with-slots (state avail-graph)
      player
    (if (get-edge avail-graph src trgt)
        (make-instance 'claim
                       :source src
                       :target trgt
                       :punter (id state))
        (make-instance 'option
                       :source src
                       :target trgt
                       :punter (id state))))
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
   (cluster :initarg :cluster
            :accessor cluster
            :initform nil)))

(defun make-location (kind node &rest params)
  (apply #'make-instance 'location
         :kind kind
         :node node
         params))

(defun mine-location? (location)
  (eq (kind location) :mine))

(defclass connector-player (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil)
   (avail-option-graph :accessor avail-option-graph
                       :initform nil)
   (avail-options :accessor avail-options
                  :initform 0)
   (current-network :accessor current-network
                    :initform nil)
   (claimed-mines :accessor claimed-mines
                  :initform nil)
   (locations-table :accessor locations-table
                    :initform nil)
   (gambling :accessor gambling
             :initform nil
             :initarg :gambling)
   (tricky :accessor tricky
           :initform nil
           :initarg :tricky)
   (use-options :accessor use-options
                :initform t
                :initarg :use-options)
   (smart-options :accessor smart-options
                  :initform t
                  :initarg :smart-options)))

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

(defun hash-tables-clear-intersections (big-tab small-tab func)
  (maphash (lambda (key val)
             (when (gethash key big-tab)
               (funcall func key val)
               (remhash key small-tab)))
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
                                  (unless (gethash neighbour cluster)
                                    (setf (gethash neighbour adj) t)))))
             cluster)
    (hash-table-count adj)))

(defun extend-cluster (graph cluster)
  (maphash (lambda (node val)
             (declare (ignore val))
             (mapc-node-edges graph node
                              (lambda (neighbour data)
                                (declare (ignore data))
                                (unless (gethash neighbour cluster)
                                  (return-from extend-cluster (values node neighbour))))))
           cluster))

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
                           (cons node acc)
                           (%move-from node1 (cons node acc))))))
            (multiple-value-bind (path-found target)
                (%bfs nodes)
              (if path-found
                  (%move-from target nil)
                  nil)))))))

(defun estimate-node-score (node claimed-mines distance-tab)
  (loop
     :for claimed-mine :in claimed-mines
     :for val = (gethash (list claimed-mine node) distance-tab)
     :when val
     :summing (* val val)))

(defun node-with-max-distance (mine distance-tab node->moves moves-cap)
  (let ((result-node nil)
        (max-dist 0))
    (maphash (lambda (node move-num)
               (when (< move-num moves-cap)
                 (let ((dist (gethash (list mine node) distance-tab))
                       (rev-dist (gethash (list node mine) distance-tab)))
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
        (claimed-mines nil)
        (moves-cap (floor (* 0.2 num-moves))))
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
             (%gamble ()
               (let ((futures nil))
                 (loop :for mine :in claimed-mines
                    :when (< (or (gethash mine move-num-tab)
                                 1000000000)
                             moves-cap)
                    :do (multiple-value-bind (node dist)
                            (node-with-max-distance mine distance-tab
                                                    move-num-tab moves-cap)
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

(defparameter *seconds-to-choose* 7)

(defun find-best-mines-order (graph distance-tab mines num-moves &key gamble)
  (let ((max-score 0)
        (best-order nil)
        (best-futures nil)
        (panic-time (+ (get-internal-run-time)
                       (* *seconds-to-choose*
                          internal-time-units-per-second))))
    (loop :for mine :in mines
       :do (multiple-value-bind (order score futures)
               (build-full-network graph distance-tab mine mines num-moves
                                   :gamble gamble)
             (when (> score max-score)
               (setf max-score score)
               (setf best-order order)
               (setf best-futures futures)
               (when (>= (get-internal-run-time)
                         panic-time)
                 ;; (format t "Start timed out: ~%")
                 (return)))))
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

(defmethod make-player ((player-class (eql 'connector-player)) &rest params)
  (apply #'make-instance 'connector-player params))

(defun clear-state (state)
  (setf (game-map state) nil)
  (setf (sites state) nil))

(defun mines-table (mines)
  (let ((tab (make-hash-table :test #'equal)))
    (loop :for mine :in mines
       :do (setf (gethash mine tab) t))
    tab))

(defmethod init-player :after ((player connector-player) setup-message)
  (with-slots (avail-graph avail-option-graph
                           avail-options
                           state current-network gambling tricky futures
                           locations-table
                           use-options)
      player
    (setf avail-graph (clone-graph (game-map state)))
    (when (setup-settings setup-message)
      (unless (settings-options (setup-settings setup-message))
        (setf use-options nil))
      (unless (settings-futures (setup-settings setup-message))
        (setf gambling nil)))
    (when use-options
      (setf avail-option-graph (clone-graph avail-graph))
      (setf avail-options (length (mines state))))
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
           (mines-tab (mines-table mines)))
      (multiple-value-bind (sorted cool-futures)
          (find-best-mines-order avail-graph (distance-tab state)
                                 mines (floor (length (map-rivers (setup-map setup-message)))
                                              (players-number state))
                                 :gamble gambling)
        (when cool-futures
          (setf futures
                (loop :for (mine node _) :in cool-futures
                   :collect (make-instance 'future
                                           :source mine
                                           :target node))))
        (setf locations-table (make-hash-table :test #'equal))
        (loop :for location :in sorted
           :do (setf (gethash location locations-table)
                     (if (gethash location mines-tab)
                         :mine
                         :future)))
        ;; Start from first
        (setf (gethash (car sorted) current-network) t)
        (clear-state state)))))

(defmethod update-player :after ((player connector-player) moves)
  (with-slots (current-network avail-graph avail-option-graph
                               avail-options
                               state locations-table tricky
                               use-options)
      player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (if (get-edge avail-graph src trgt)
           (remove-edge avail-graph src trgt)
           (when use-options
             (when (= id (id state))
               (decf avail-options))
             (remove-edge avail-option-graph src trgt)))
       (when (= id (id state))
         (if (or (gethash src current-network)
                 (gethash trgt current-network))
             (progn
               (setf (gethash src current-network) t)
               (setf (gethash trgt current-network) t))
             (when (or (gethash src locations-table)
                       (gethash trgt locations-table))
               (setf (gethash src locations-table) t)
               (setf (gethash trgt locations-table) t))))))))

(defun extract-first (list predicate &optional acc)
  (if (null list)
      (values nil (reverse acc))
      (if (funcall predicate (car list))
          (values (car list) (append (reverse acc) (cdr list)))
          (extract-first (cdr list) predicate (cons (car list) acc)))))

(defun filter-alive-locs (graph table)
  (maphash (lambda (node kind)
             (declare (ignore kind))
             (unless (any-neighbour graph node)
               (remhash node table)))
           table))

(defun sub-cluster-moves (graph start-node cluster visited)
  (let ((adj (make-hash-table :test #'equal)))
    (labels ((%node (node)
               (mapc-node-edges
                graph node
                (lambda (neighbour data)
                  (declare (ignore data))
                  (unless (gethash neighbour cluster)
                    (setf (gethash neighbour adj) node)))))
             (%nodes (nodes)
               (let ((next-nodes nil))
                 (loop :for node :in nodes
                    :do (unless (gethash node visited)
                          (setf (gethash node visited) t)
                          (%node node)
                          (mapc-node-edges
                           graph node
                           (lambda (neighbour data)
                             (declare (ignore data))
                             (when (and (gethash neighbour cluster)
                                        (not (gethash neighbour visited)))
                               (push neighbour next-nodes))))))
                 (when next-nodes
                   (%nodes next-nodes)))))
      (%nodes (list start-node)))
    adj))

(defmethod select-move ((player connector-player))
  (with-slots (avail-graph
               avail-option-graph
               avail-options
               current-network locations-table
               state claimed-mines
               tricky smart-options)
      player
    (labels ((%claim (location kind)
               (when (eq kind :mine)
                 (push location claimed-mines)))
             (%do-move ()
               (if tricky
                   (%tricky-check-locations)
                   (%do-move-targeted)))
             (%too-narrow? (tab)
               (let ((freedom (hash-table-count tab)))
                 (and (> freedom 0)
                      (< freedom 3))))
             (%any-move (tab)
               (maphash (lambda (node from-node)
                          (return-from %any-move (values from-node node)))
                        tab))
             (%tricky-check-locations ()
               (let ((visted (make-hash-table :test #'equal)))
                 (maphash
                  (lambda (node kind)
                    (declare (ignore kind))
                    (unless (gethash node visted)
                      (let ((moves (sub-cluster-moves avail-graph node locations-table visted)))
                        (when (%too-narrow? moves)
                          (multiple-value-bind (src trgt)
                              (%any-move moves)
                            (player-log player "Tricky freedom: ~A, extending: ~A -> ~A~%"
                                        (hash-table-count moves) src trgt)
                            (return-from %tricky-check-locations (make-claim-player player src trgt)))))))
                  locations-table))
               (%do-move-targeted))
             (%path-num-options (path)
               (loop :for (from to) :on path
                  :when to
                  :summing (if (get-edge avail-graph from to)
                               0
                               1)))
             (%find-targeted-move ()
               (let ((path (find-regions-connecting-move avail-graph current-network locations-table)))
                 (if (and (null path)
                          (> avail-options 0))
                     (if smart-options
                         (let ((paths (k-shortest-between-regions avail-option-graph 3 '(:a :b)
                                                                  current-network locations-table)))
                           (car (sort (copy-list paths) #'<
                                      :key #'%path-num-options)))
                         (find-regions-connecting-move avail-option-graph current-network locations-table))
                     path)))
             (%do-move-targeted ()
               (if (> (hash-table-count locations-table) 0)
                   (progn
                     (hash-tables-clear-intersections
                      current-network
                      locations-table
                      (lambda (node kind)
                        (%claim node kind)))
                     (let ((path (%find-targeted-move)))
                       (player-log player "Connecting ~A and ~A : ~A~%" (alexandria:hash-table-keys current-network)
                                   (alexandria:hash-table-keys locations-table)
                                   path)
                       (cond
                         ((null path)
                          (%do-random-move))
                         ((null (cdr path))
                          (%claim (car path) (gethash (car path) locations-table))
                          (remhash (car path) locations-table)
                          (%do-move-targeted))
                         (t (make-claim-player player (first path) (second path))))))
                   (%do-random-move)))
             (%do-random-move ()
               (let ((max-move (find-best-non-targeted-move avail-graph current-network
                                                            (distance-tab state) claimed-mines)))
                 (player-log player "Random move : ~A~%" max-move)
                 (if max-move
                     (make-claim-player player (car max-move) (cdr max-move))
                     (%reset-targets))))
             (%reset-targets ()
               (filter-alive-locs avail-graph locations-table)
               (if (> (hash-table-count locations-table) 0)
                   (progn
                     ;; Reset to another cluster
                     (player-log player "Reset to another cluster, alive objects: ~A~%"
                                 (alexandria:hash-table-plist locations-table))
                     (let ((start
                            (block find-mine
                              (maphash (lambda (node kind)
                                         (when (eq kind :mine)
                                           (return-from find-mine node)))
                                       locations-table))))
                       (if start
                           (progn
                             (player-log player "Cluster : ~A -> ~A~%"
                                         start (alexandria:hash-table-keys locations-table))
                             (setf (gethash start current-network) t)
                             ;; Another cluster - another score
                             (setf claimed-mines nil)
                             (%do-move))
                           (progn
                             (player-log player "Give up :~%")
                             ;; Give up
                             (clrhash locations-table)
                             (%do-totally-random-move))))
                     (%do-move))
                   (%do-totally-random-move)))
             (%do-totally-random-move ()
               (let ((node (any-node avail-graph)))
                 (if node
                     (make-claim-player player node (any-neighbour avail-graph node))
                     (make-pass state)))))
      (%do-move))))
