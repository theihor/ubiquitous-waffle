(uiop:define-package :src/rl/player
    (:use :common-lisp
          :src/game-player
          :src/graph
          :src/game-state)
  (:export #:edge->free
           #:group->edges))

(in-package :src/rl/player)

(defparameter *groups-n* 4)

(defclass rl-player (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil)
   (edge->free :accessor edge->free
               :initform (make-hash-table :test #'equal))
   (group->edges :accessor group->edges
                 :initform (make-hash-table :test #'equal))))

(defmethod make-player ((player-class (eql 'rl-player)) &rest params)
  (declare (ignore params))
  (make-instance 'rl-player))

(defun compute-edge-groups (player-state)
  (let ((d-tab (distance-tab (state player-state)))
        (site->score (make-hash-table :test #'equal))
        (edge->score (make-hash-table :test #'equal))
        (min-edge-score nil)
        (max-edge-score nil))
    
    (maphash (lambda (edge d)
               (incf (gethash (cdr edge) site->score 0) (* d d)))
             d-tab)
    (mapc-all-edges
     (game-map (state player-state))
     (lambda (source target data)
       (declare (ignore data))
       (when (< source target)
         (let ((score (+ (gethash source site->score 0)
                         (gethash target site->score 0))))
           (setf (gethash (cons source target) edge->score) score)
           (when (or (null max-edge-score)
                     (> score max-edge-score))
             (setf max-edge-score score))
           (when (or (null min-edge-score)
                     (< score min-edge-score))
             (setf min-edge-score score)))
         ;; (format t "~A->~A = ~A~%" source target
         ;;         (gethash (cons source target) edge->score))
         )))
    ;; (format t "min: ~A; max: ~A~%" min-edge-score max-edge-score)
    (let ((range-size (/ (+ 1e-2 (- max-edge-score min-edge-score)) *groups-n*))
          (edge->group (make-hash-table :test #'equal)))
      (setf (group->edges player-state) (make-hash-table :test #'equal)) 
      (maphash (lambda (edge score)
                 (setf (gethash edge edge->group)
                       (floor (/ (- score min-edge-score) range-size))))
               edge->score)
      (maphash (lambda (edge gr)
                 (push edge (gethash gr (group->edges player-state))))
               edge->group))))

(defmethod init-player :after ((player rl-player) setup-message)
  (with-slots (avail-graph state edge->free) player
    (setf avail-graph (clone-graph (game-map state)))
    (mapc-all-edges (game-map (state player))
                    (lambda (source target data)
                      (when (eq data :free)
                        (setf (gethash (cons source target) edge->free) t))))
    (compute-edge-groups player)))

(defmethod update-player :after ((player rl-player) moves)
  (with-slots (avail-graph state edge->free) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (declare (ignore id))
       (remove-edge avail-graph src trgt)
       (remhash (cons src trgt) edge->free)
       (compute-edge-groups player)))))


