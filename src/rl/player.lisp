(uiop:define-package :src/rl/player
    (:use :common-lisp
          :src/game-player
          :src/graph
          :src/game-state
          :src/utils)
  (:import-from :src/punter
                :punter-graph)
  (:export #:edge->group
           #:group->edges
           #:avail-graph
           #:rl-player
           #:*groups-n1*
           #:*groups-n2*
           #:clone-player))

(in-package :src/rl/player)

(defparameter *groups-n1* 4)
(defparameter *groups-n2* 4)

(defclass rl-player (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil
                :initarg :avail-graph)
   (edge->group :accessor edge->group
                :initform (make-hash-table :test #'equal)
                :initarg :edge->group)
   (group->edges :accessor group->edges
                 :initform (make-hash-table :test #'equal)
                 :initarg :group->edges)
   (edge->score :accessor edge->score)
   (min-edge-score :accessor min-edge-score)
   (max-edge-score :accessor max-edge-score)
   (min-edge-dist :accessor min-edge-dist)
   (max-edge-dist :accessor max-edge-dist)
   ))

(defmethod make-player ((player-class (eql 'rl-player)) &rest params)
  (declare (ignore params))
  (make-instance 'rl-player))

(defun compute-edge->score (player-state)
  (let ((d-tab (distance-tab (state player-state)))
        (site->score (make-hash-table :test #'equal))
        (edge->score (make-hash-table :test #'equal))
        (min-edge-score nil)
        (max-edge-score nil))

    ;; compute edge->score with site->score
    ;; and min-edge-score and max-edge-score
    (maphash (lambda (edge d)
               (incf (gethash (cdr edge) site->score 0) (* d d)))
             d-tab)
    (mapc-all-edges
     (avail-graph player-state)
     (lambda (source target data)
       (declare (ignore data)) 
       (let ((score (+ (gethash source site->score 0)
                       (gethash target site->score 0))))
         (setf (gethash (cons source target) edge->score) score)
         (when (or (null max-edge-score)
                   (> score max-edge-score))
           (setf max-edge-score score))
         (when (or (null min-edge-score)
                   (< score min-edge-score))
           (setf min-edge-score score)))))

    (setf (min-edge-score player-state) min-edge-score)
    (setf (max-edge-score player-state) max-edge-score)
    (values edge->score min-edge-score max-edge-score)))

(defun compute-edge->dist (player-state)
  (let ((edge->dist (make-hash-table :test #'equal))
        (claimed-cluster (get-nodes
                          (punter-graph (elt (punters (state player-state))
                                             (id (state player-state))))))
        (min-d nil)
        (max-d nil))
        
    (mapc-all-edges
     (avail-graph player-state)
     (lambda (src tgt s)
       (declare (ignore s))
       (let ((target-cluster (make-hash-table :test #'equal)))
         (setf (gethash src target-cluster) t)
         (setf (gethash tgt target-cluster) t)
         (let* ((path (find-regions-connecting-move (avail-graph player-state)
                                                    claimed-cluster
                                                    target-cluster))
                (n (if path (1- (length path)) t)))
           (unless (eq n t)
             (when (or (null min-d) (< n min-d)) (setf min-d n))
             (when (or (null max-d) (> n max-d)) (setf max-d n)))
           (setf (gethash (cons src tgt) edge->dist) n)))))

    (values edge->dist min-d max-d)
    ))

(defun compute-edge-groups (player-state &key (recompute nil))
  "Destructively computes group->edges and edge->group for player-state"
  (setf (group->edges player-state) (make-hash-table :test #'equal))
  (setf (edge->group player-state) (make-hash-table :test #'equal)) 

  (multiple-value-bind (edge->score min-score max-score)
      (if recompute
          (compute-edge->score player-state)
          (values (edge->score player-state)
                  (min-edge-score player-state)
                  (max-edge-score player-state)))
    (when recompute
      (setf (min-edge-score player-state) min-score)
      (setf (max-edge-score player-state) max-score)
      (setf (edge->score player-state) edge->score))

    ;; edge->dist table should be recomputed every time
    (multiple-value-bind (edge->dist min-d max-d)
        (compute-edge->dist player-state)

      (setf (min-edge-dist player-state) min-d)
      (setf (max-edge-dist player-state) max-d) 

      ;; (when (and (null max-edge-score)
      ;;            (null min-edge-score))
      ;;   (return-from compute-edge-groups nil))
      

;; (format t "~%~%")
      ;; compute edge->group and group->edge
      (let ((range-size1 (/ (+ 1e-2 (- max-score min-score)) *groups-n1*))
            (range-size2 (when (and max-d min-d)
                           (/ (+ 1e-2 (- max-d min-d)) (1- *groups-n2*)))))
        
        (maphash (lambda (edge d)
                   (let* ((score (gethash edge edge->score))
                          (g1 (floor (/ (- score min-score) range-size1)))
                          (g2 (if (eq d t)
                                  (1- *groups-n2*)
                                  (floor (/ (- d min-d) range-size2)))))
                     ;; (format t "edge: ~A d = ~A; score = ~A; g1 = ~A; g2 = ~A~%"
                     ;;         edge d score g1 g2)
                     (setf (gethash edge (edge->group player-state))
                           (+ (* g1 4) g2))))
                 edge->dist)
        
        (maphash (lambda (edge gr)
                   (push edge (gethash gr (group->edges player-state))))
                 (edge->group player-state)))

      ;; (maphash (lambda (g e) (format t "~A: ~A~%" g e)) (group->edges player-state))
      ))) 

(defmethod init-player ((player rl-player) setup-message)
  (setf (state player) (make-game-state setup-message
                                        :type 'src/game-state:game-with-scores)))

(defmethod init-player :after ((player rl-player) setup-message)
  (with-slots (avail-graph state edge->group) player
    (setf avail-graph (clone-graph (game-map state)))
    (compute-edge-groups player :recompute t)))

(defmethod update-player :after ((player rl-player) moves)
  (with-slots (avail-graph state edge->group group->edges) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (declare (ignore id))
       (remove-edge avail-graph src trgt)
       ;; (let* ((g (gethash (cons src trgt) edge->group))
       ;;        (lst (remove (cons src trgt) (gethash g group->edges) :test #'equal)))
       ;;   (remhash (cons src trgt) edge->group)
       ;;   (if lst
       ;;       (setf (gethash g group->edges) lst)
       ;;       (remhash g group->edges)))
       
       (compute-edge-groups player)
       ))))

(defun clone-player (player-state)
  (copy-instance player-state
                 :state (clone-game (state player-state))
                 :avail-graph (clone-graph (avail-graph player-state))
                 :edge->group (copy-hash-table
                               (edge->group player-state))
                 :group->edges (copy-hash-table
                                (group->edges player-state)
                                :val-copy-func #'copy-list)))
