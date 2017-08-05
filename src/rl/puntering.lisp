(uiop:define-package :src/rl/puntering
    (:use :common-lisp :cl-rl
          :anaphora
          :src/graph
          :src/game-state
          :src/game-player
          :src/decode
          :src/rl/player))

(in-package :src/rl/puntering)

(defclass puntering (cl-rl::markov-desicion-process)
  ((setup-message :initarg :setup-message
                  :reader setup-message)))

(defparameter *actions*
  (loop :for i :from 0 :to *groups-n* :collect i))

(defmethod possible-actions ((p puntering) player-state)
  *actions*)

(defmethod perform-action ((p puntering) player-state action)
  (let* ((prev-score (score (state player-state)))
         (edge (aif (gethash action (group->edges p))
                    (cl-rl::random-choose it)
                    (cl-rl::random-choose (alexandria:hash-table-keys
                                           (edge->free player-state)))))
         (move (make-claim (state player-state) (car edge) (cdr edge))))
    (update-player player-state (list move))
    (values player-state (- (score (state player-state))
                            prev-score))))

(defmethod initial-state ((p puntering))
  (let ((player-state 'rl-player))
    (init-player player-state (parse (setup-message p)))
    player-state))

(defmethod terminal-state? ((p puntering) player-state)
  (= 0 (hash-table-count (edge->free player-state))))

(defmethod state-features ((p puntering) player-state)
  (let ((total 0)
        (lst nil))
    (maphash (lambda (g edges)
               (push (length edges) lst)
               (incf (length edges) total))
             (group->edges player-state))
    (mapcar (if (= 0 total)
                (constantly 0)
                (lambda (n) (/ n total)))
            lst)))

;; (defmethod estimate-action ((p puntering) player-state action)
;;   (let ((edge (aif (gethash action (group->edges p))
;;                    (cl-rl::random-choose it)
;;                    (cl-rl::random-choose (free-edges player-state)))))
;;     (list (list ))
;;     (make-claim (state player-state) (car edge) (cdr edge))))

