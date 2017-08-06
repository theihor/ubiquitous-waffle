(uiop:define-package :src/rl/player
    (:use :common-lisp
          :src/game-player
          :src/graph
          :src/game-state)
  (:export #:edge->group
           #:group->edges
           #:avail-graph
           #:rl-player))

(in-package :src/rl/player)

(defparameter *groups-n* 4)

(defclass rl-player (game-player)
  ((avail-graph :accessor avail-graph
                :initform nil
                :initarg :avail-graph)
   (edge->group :accessor edge->group
                :initform (make-hash-table :test #'equal)
                :initarg :edge->group)
   (group->edges :accessor group->edges
                 :initform (make-hash-table :test #'equal)
                 :initarg :group->edges)))

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
    
    (format t "min: ~A; max: ~A~%" min-edge-score max-edge-score)
    
    (setf (group->edges player-state) (make-hash-table :test #'equal))
    (setf (edge->group player-state) (make-hash-table :test #'equal))
    (when (and (null max-edge-score)
               (null min-edge-score))
      (return-from compute-edge-groups nil))
    (let ((range-size (/ (+ 1e-2 (- max-edge-score min-edge-score)) *groups-n*)))
       
      (maphash (lambda (edge score)
                 (setf (gethash edge (edge->group player-state))
                       (floor (/ (- score min-edge-score) range-size))))
               edge->score)
      
      (maphash (lambda (edge gr)
                 (push edge (gethash gr (group->edges player-state))))
               (edge->group player-state)))
     (maphash (lambda (g e) (format t "~A: ~A~%" g e)) (group->edges player-state))
    ))

(defmethod init-player ((player rl-player) setup-message)
  (setf (state player) (make-game-state setup-message
                                        :type 'src/game-state:game-with-scores)))

(defmethod init-player :after ((player rl-player) setup-message)
  (with-slots (avail-graph state edge->group) player
    (setf avail-graph (clone-graph (game-map state)))
    (compute-edge-groups player)))

(defmethod update-player :after ((player rl-player) moves)
  (with-slots (avail-graph state edge->group group->edges) player
    (mapc-claims
     moves
     (lambda (id src trgt)
       (declare (ignore id))
       (remove-edge avail-graph src trgt)
       (let* ((g (gethash (cons src trgt) edge->group))
              (lst (remove (cons src trgt) (gethash g group->edges) :test #'equal)))
         (remhash (cons src trgt) edge->group)
         (if lst
             (setf (gethash g group->edges) lst)
             (remhash g group->edges))) 
       ;; (compute-edge-groups player)
       ))))


