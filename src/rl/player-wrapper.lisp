(uiop:define-package :src/rl/player-wrapper
    (:use :common-lisp :cl-rl
          :anaphora
          :src/graph
          :src/game-state
          :src/game-player
          :src/decode
          :src/rl/player
          :src/rl/puntering
          :src/utils))

(in-package :src/rl/player-wrapper)

(defparameter *alpha* 0.1)
(defparameter *gamma* 0.99)
(defparameter *td-lambda* 0.7)

(defclass rl-player-wrapper ()
  ((agent :accessor agent
          :initarg :agent
          :initform nil)
   (puntering :accessor puntering)
   (state :initarg :state
          :accessor state
          :documentation "rl-player")
   (total-reward :accessor total-reward
                 :initform 0)
   (current-step :accessor current-step
                 :initform 0)
   (theta :initarg :theta
          :reader theta
          :initform nil)))

(defmethod make-player ((player-class (eql 'rl-player-wrapper)) &rest params)
  (if params
      (let* ((fname (car params))
             (theta nil)
             (in (open fname)))
        (when in
          (loop for line = (read-line in nil) 
             while line do
               (push (read-from-string line) theta))
          (close in))
        (make-instance 'rl-player-wrapper :theta (reverse theta)))
      (make-instance 'rl-player-wrapper))) 

(defmethod init-player ((p rl-player-wrapper) m)
  (let* ((problem (make-instance 'puntering :message m))
         (agent (cl-rl::make-totd-lambda-agent
                 problem
                 :alpha *alpha* :gamma *gamma* :td-lambda *td-lambda*
                 :theta (theta p))))
    (setf (puntering p) problem)
    (setf (agent p) agent)
    (setf (state p) (initial-state problem))))

(defmethod update-player ((p rl-player-wrapper) m)
  (with-slots (puntering agent state current-step total-reward) p
    (setf (message puntering) m)
    (let* ((action (cl-rl::agent-action agent puntering state)))
      (multiple-value-bind (next-state reward)
          ;; action ignored there, since update relays on message
          (perform-action puntering state action)
        (incf total-reward
              (* (expt (cl-rl::agent-gamma agent) current-step)
                 reward))
        (incf current-step)
        (cl-rl::agent-update-policy agent puntering reward action state next-state)
        (setf state next-state)))))

(defmethod select-move ((p rl-player-wrapper))
  (with-slots (agent puntering state) p
    (let* ((action (cl-rl::agent-action agent puntering state))
           (move (make-move-from-action state action)))
      move)))

(defun dump-theta-to-file (player-wrapper file)
  (with-open-file (f file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (loop :for w :in (cl-rl::agent-theta (agent player-wrapper)) :do
       (format f "~A~%" w))))

(defmethod player-futures ((p rl-player-wrapper))
  nil)
