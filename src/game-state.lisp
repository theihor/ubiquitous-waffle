(uiop:define-package :src/game-state
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/punter
          :src/bfs
          :src/utils)
  (:export #:make-game-state
           #:process-moves
           #:game-map
           #:mines
           #:id
           #:players-number
           #:mapc-claims
           #:punters
           #:distance-tab
           #:dump-state
           #:game
           #:game-with-scores
           #:clone-game
           #:score))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-state)

(defclass game ()
  ((game-map :initarg :game-map
             :accessor game-map)
   (mines :initarg :mines
          :reader mines
          :type list)
   (sites :initarg :sites
          :reader sites
          :type list)
   (id :initarg :id
       :reader id
       :type integer)
   (players-number :initarg :players-number
                   :reader players-number
                   :type integer)
   (distance-tab :accessor distance-tab
                 :documentation "Map (mine . target) -> distance")))

(defgeneric clone-game (game))

(defmethod clone-game (game)
  (copy-instance game))

(defmethod clone-game :after ((game game))
  (setf (game-map game) (clone-graph (game-map game)) ))

(defgeneric process-moves (state moves))
(defgeneric dump-state (state moves))

(defun make-game-state (setup-message &key (type 'game))
  (let ((punters-number (setup-punters setup-message)))
    (make-instance
     type
     :id (setup-punter setup-message)
     :players-number punters-number
     :mines (map-mines (setup-map setup-message))
     :sites (map-sites (setup-map setup-message))
     :game-map (build-map (setup-map setup-message)))))

(defmethod initialize-instance :after ((state game) &key)
  (setf (distance-tab state)
        (bfs:multiple-bfs-distances (game-map state) (mines state))))

(defun build-map (the-map)
  (let ((g (make-graph 'hash-graph)))
    (dolist (river (map-rivers the-map))
      (let ((src (river-source river))
            (tgt (river-target river)))
        (assert (integerp src))
        (assert (integerp tgt))
        (add-edge g src tgt :free)))
    g))

(defun mapc-claims (moves func)
  (dolist (m moves)
    (etypecase m
      (pass nil)
      (claim
       (let ((id (move-punter m))
             (src (claim-source m))
             (tgt (claim-target m)))
         (assert (integerp id))
         (assert (integerp src))
         (assert (integerp tgt))
         (funcall func id src tgt))))))

(defmethod process-moves ((state game) moves)
  (let ((the-map (game-map state)))
    (mapc-claims
     moves
     (lambda (id src tgt)
       (assert (eq (get-edge the-map src tgt) :free))
       (add-edge the-map src tgt id)))))

(defclass game-with-scores (game)
  ((punters :initarg :punters
            :accessor punters)))

(defmethod initialize-instance :after ((state game-with-scores) &key)
  (setf (distance-tab state)
        (bfs:multiple-bfs-distances (game-map state) (mines state)))
  (setf (punters state)
        (make-array
         (list (players-number state))
         :initial-contents (loop :for id :from 0 :below (players-number state) :collect
                              (make-instance 'punter
                                             :id id
                                             :graph (make-graph 'hash-graph)
                                             :mines (mines state)
                                             :sites (sites state))))))

(defmethod clone-game :after ((state game-with-scores))
  (setf (punters state)
        (make-array (array-dimensions (punters state))
                    :initial-contents (mapcar #'clone-punter (coerce (punters state) 'list)))))

(defun clone-punter (punter)
  (copy-instance
   punter
   :graph (clone-graph (punter-graph punter))
   :mine->sites (copy-hash-table
                 (src/punter::mine->sites punter)
                 :val-copy-func #'copy-hash-table)
   :site->mines (copy-hash-table
                 (src/punter::site->mines punter)
                 :val-copy-func #'copy-hash-table)
   ))

(defmethod process-moves ((state game-with-scores) moves)
  (let ((the-map (game-map state)))
    (mapc-claims
     moves
     (lambda (id src tgt)
       (assert (eq (get-edge the-map src tgt) :free))
       (claim-edge (elt (punters state) id) src tgt (distance-tab state))
       (add-edge the-map src tgt id)))))

(defmethod score ((state game-with-scores))
  (score (elt (punters state) (id state))))

(defun dump-graph-with-distances (state file)
  (let ((rev-dist (make-hash-table :test #'equal)))
    (maphash (lambda (mine-node d)
               (let ((tab (or (gethash (cdr mine-node) rev-dist)
                              (make-hash-table))))
                 (setf (gethash (car mine-node) tab) d)
                 (setf (gethash (cdr mine-node) rev-dist) tab)))
             (distance-tab state))
    (graph::graph->dot
     (game-map state) file
     :node-label (lambda (n)
                   (format nil "\"~A\""(alexandria:hash-table-values
                                        (gethash n rev-dist)))))))

(defparameter *dot-colors*
  '(blue red green orange bisque3   	black 	
    blue1 	blue2 	blue3 	 	blueviolet
    brown 	brown3 	brown4
    burlywood 	 	burlywood4
    cadetblue 	 	cadetblue4
    chartreuse 	 	chartreuse4
    chocolate 	chocolate1 	
    coral 	 	coral4))

(defmethod dump-state ((state game-with-scores) file)
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format s "graph g {~%")
    (loop :for p :across (punters state)
       :for i :from 0
       :do (let ((color (format nil "~A" (nth i *dot-colors*))))
             (maphash (lambda (source targets)
                        (maphash
                         (lambda (target data)
                           (declare (ignore data))
                           (when (< source target)
                             (format s "~A -- ~A [color=~A];~%"
                                     source target color)))
                         targets))
                      (graph-edges (punter-graph p)))))
    (format s "}~%")))
