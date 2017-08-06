(uiop:define-package :src/rl/run
    (:use :common-lisp
          :src/rl/puntering
          :src/rl/player
          :cl-rl))

(in-package :src/rl/run)

(defun run-punter-totd-lambda-solver (setup-message &key (alpha 0.4) (gamma 0.99) (td-lambda 0.8) (agent nil))
  (let* ((problem (make-instance 'puntering :message setup-message))
         (agent (or agent
                    (cl-rl::make-totd-lambda-agent
                     problem :alpha alpha :gamma gamma :td-lambda td-lambda))))
    (run-mdp-with-agent problem agent
                        :max-steps nil
                        :discount-factor gamma
                        :report-func (lambda (i agent mdp total-reward
                                         reward action state next-state) 
                                       (declare (ignorable
                                                 i agent mdp total-reward reward
                                                 action state next-state))
                                       ;; (format t "action: ~A, move: ~A; "
                                       ;;         action (src/rl/puntering::make-move-from-action state action))
                                       (format t "reward: ~A; total-reward: ~A~%"
                                               reward total-reward)
                                       (maphash (lambda (g e) (format t "~A: ~A~%" g e)) (group->edges state))
                                       ;; (format  t "theta: ~A~%" (cl-rl::agent-theta agent))
                                       ))
    agent))
