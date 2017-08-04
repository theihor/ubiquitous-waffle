(uiop:define-package :src/game-state
    (:use :common-lisp
          :src/game-protocol
          :src/graph)
  (:export #:make-game-state
           #:process-moves
           #:game-map
           #:mines
           #:id
           #:players-number))

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
                   :type integer)))

(defun make-game-state (setup-message)
  (let ((sites-number (map-sites (setup-map setup-message))))
    (assert (integerp sites-number))
    (make-instance
     'game
     :id (setup-punter setup-message)
     :players-number (setup-punters setup-message)
     :mines (mapcar (lambda (x)
                      (assert (integerp x))
                      (assert (< x sites-number))
                      x)
                    (map-mines (setup-map setup-message)))
     :game-map (build-map (setup-map setup-message)
                          sites-number))))

(defun build-map (the-map num-nodes)
  (let ((g (make-graph 'array-graph :num-nodes num-nodes)))
    (dolist (river (map-rivers the-map))
      (let ((src (river-source river))
            (tgt (river-target river)))
        (assert (integerp src))
        (assert (integerp tgt))
        (add-edge g src tgt :free)))
    g))

(defun process-moves (state moves)
  (let ((the-map (game-map state)))
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
           (assert (eq (get-edge the-map src tgt) :free))
           (add-edge the-map src tgt id)))))))
