(uiop:define-package :src/rl/puntering
    (:use :common-lisp :cl-rl
          :anaphora
          :src/graph
          :src/game-state
          :src/game-player
          :src/decode
          :src/rl/player
          :src/utils)
  (:export #:puntering
           #:message
           #:make-move-from-action))

(in-package :src/rl/puntering)

(defclass puntering (markov-decision-process)
  ((message :initarg :message
            :accessor message)))

(defmethod possible-actions ((p puntering) player-state)
  (alexandria:hash-table-keys (group->edges player-state)))

(defun make-move-from-action (player-state action)
  (let* ((edge (aif (gethash action (group->edges player-state))
                    (cl-rl::random-choose it)
                    :pass
                    ;; (aif (alexandria:hash-table-keys (edge->group player-state))
                    ;;      (cl-rl::random-choose it)
                    ;;      :pass)
                    ))
         (move (if (eq edge :pass)
                   (make-pass (state player-state))
                   (make-claim (state player-state) (car edge) (cdr edge)))))
    move))

(defmethod perform-action ((p puntering) player-state action)
  "Update player-state using (message p)"
  (declare (ignore action))
  (let* ((prev-score (score (state player-state)))
         ;; (move (make-move-from-action player-state action))
         (next-state (clone-player player-state)))
    ;; (format t "performing ~A: ~A~%" action move)
    (update-player next-state (message p))
    (let* ((reward (- (score (state next-state))
                      prev-score))
           (reward (if (= 0 reward) -1 reward)))
      (values next-state reward))))

(defparameter *initial-state* nil)

(defmethod initial-state ((p puntering))
  (or *initial-state*
      (let ((player-state (make-player 'rl-player)))
        (init-player player-state (message p))
        (setf *initial-state* player-state))))

(defmethod terminal-state? ((p puntering) player-state)
  (= 0 (hash-table-count (edge->group player-state))))

(defmethod state-features ((p puntering) player-state)
  (with-slots (group->edges) player-state
    (let* ((total 0)
           (lst (loop :for i :from 0 :below (* *groups-n1* *groups-n2*) :collect
                   (let ((n (length (gethash i group->edges))))
                     (incf total n)
                     n))))
      (mapcar (if (= 0 total)
                  (constantly 0)
                  (lambda (n) (/ n total)))
              lst))))

(defmethod estimate-action ((p puntering) player-state action)
  ;; (format t "estimating ~A~%" action)
  (let* ((prev-score (score (state player-state)))
         (move (make-move-from-action player-state action))
         (new-state (clone-player player-state)))
    (update-player new-state (list move))
    (let* ((reward (- (score (state player-state))
                      prev-score))
           (reward (if (= 0 reward) -0.1 reward)))
      (list (list new-state reward 1.0)))))

