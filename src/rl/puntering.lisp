(uiop:define-package :src/rl/puntering
    (:use :common-lisp :cl-rl
          :anaphora
          :src/graph
          :src/game-state
          :src/game-player
          :src/decode
          :src/rl/player
          :src/utils)
  (:export #:puntering))

(in-package :src/rl/puntering)

(defclass puntering (markov-decision-process)
  ((setup-message :initarg :setup-message
                  :reader setup-message)))

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
  (format t "performing ~A~%" action)
  (let* ((prev-score (score (state player-state)))
         (move (make-move-from-action player-state action)))
    (update-player player-state (list move))
    (values player-state (- (score (state player-state))
                            prev-score))))

(defparameter *initial-state* nil)

(defmethod initial-state ((p puntering))
  (or *initial-state*
      (let ((player-state (make-player 'rl-player)))
        (init-player player-state (setup-message p))
        (setf *initial-state* player-state))))

(defmethod terminal-state? ((p puntering) player-state)
  (= 0 (hash-table-count (edge->group player-state))))

(defmethod state-features ((p puntering) player-state)
  (let ((total 0)
        (lst nil))
    (maphash (lambda (g edges)
               (declare (ignore g))
               (push (length edges) lst)
               (incf total (length edges)))
             (group->edges player-state))
    (mapcar (if (= 0 total)
                (constantly 0)
                (lambda (n) (/ n total)))
            lst)))

(defmethod estimate-action ((p puntering) player-state action)
  ;; (format t "estimating ~A~%" action)
  (let* ((prev-score (score (state player-state)))
         (move (make-move-from-action player-state action))
         (new-state (copy-instance player-state
                                   :state (clone-game (state player-state))
                                   :avail-graph (clone-graph (avail-graph player-state))
                                   :edge->group (copy-hash-table
                                                 (edge->group player-state))
                                   :group->edges (copy-hash-table
                                                  (group->edges player-state)
                                                  :val-copy-func #'copy-list)
                                   )))
    (update-player new-state (list move))
    (list (list new-state (- (score (state new-state))
                             prev-score)
                1.0))))

