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
           #:mapc-claims))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/game-state)

(defclass game ()
  ((game-map :initarg :game-map
             :reader game-map)
   (mines :initarg :mines
          :reader mines
          :type list)
   (id :initarg :id
       :reader id
       :type integer)
   (players-number :initarg :players-number
                   :reader players-number
                   :type integer)
   (punters :initarg :punters
            :accessor punters)
   (distance-tab :accessor distance-tab)))

(defun make-game-state (setup-message)
  (let ((sites-number (map-sites (setup-map setup-message)))
        (punters-number (setup-punters setup-message)))
    (assert (integerp sites-number))
    (make-instance
     'game
     :id (setup-punter setup-message)
     :players-number punters-number
     :mines (mapcar (lambda (x)
                      (assert (integerp x))
                      (assert (< x sites-number))
                      x)
                    (map-mines (setup-map setup-message)))
     :game-map (build-map (setup-map setup-message)
                          sites-number)
     :punters (make-array (list punters-number)
                          :initial-contents (loop :for id :from 0 :below punters-number :collect
                                               (make-instance 'punter :id id))))))

(defmethod initialize-instance :after ((state game) &key)
  (setf (distance-tab state)
        (bfs:multiple-bfs-distances (game-map state) (mines state))))

(defun build-map (the-map num-nodes)
  (let ((g (make-graph 'array-graph :num-nodes num-nodes)))
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
