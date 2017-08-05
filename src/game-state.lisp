(uiop:define-package :src/game-state
    (:use :common-lisp
          :src/game-protocol
          :src/graph
          :src/punter
          :src/bfs)
  (:export #:make-game-state
           #:process-moves
           #:game-map
           #:mines
           #:id
           #:players-number
           #:mapc-claims
           #:punters
           #:distance-tab
           #:dump-state))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-state)

(defclass game ()
  ((game-map :initarg :game-map
             :reader game-map)
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
   (punters :initarg :punters
            :accessor punters)
   (distance-tab :accessor distance-tab
                 :documentation "Map (mine . target) -> distance")))

(defun make-game-state (setup-message)
  (let ((punters-number (setup-punters setup-message)))
    (make-instance
     'game
     :id (setup-punter setup-message)
     :players-number punters-number
     :mines (map-mines (setup-map setup-message))
     :sites (map-sites (setup-map setup-message))
     :game-map (build-map (setup-map setup-message)))))

(defmethod initialize-instance :after ((state game) &key)
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

(defun process-moves (state moves)
  (let ((the-map (game-map state)))
    (mapc-claims
     moves
     (lambda (id src tgt)
       (assert (eq (get-edge the-map src tgt) :free))
       (claim-edge (elt (punters state) id) src tgt (distance-tab state))
       (add-edge the-map src tgt id)))))

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

(defun dump-state (state file)
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
                             (format s "~A -- ~A [color=~A,label=\"~A\"];~%"
                                     source target color
                                     )))
                         targets))
                      (graph-edges (punter-graph p)))))
    (format s "}~%")))
