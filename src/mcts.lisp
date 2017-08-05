;; As described in:
;;   http://www.cameronius.com/cv/mcts-survey-master.pdf

(defpackage :src/mcts
  (:use :common-lisp)
  (:export #:select-next-move
           #:possible-actions
           #:next-state
           #:clone-state
           #:estimate-state-rewards))

(in-package :src/mcts)

(defvar *exploration-coefficient*)
(defvar *max-selection-depth*)
(defvar *players-number*)

(defclass node ()
  ((parent :reader parent
           :initarg :parent
           :type (or null node))
   (player :reader player
           :initarg :player
           :type fixnum
           :documentation "number of player whos turn it is now")
   ;; Q
   (simulation-rewards :type simple-vector
                       :initform (make-array *players-number*
                                             :element-type 'number
                                             :initial-element 0)
                       :accessor simulation-rewards
                       :initarg :simulation-rewards
                       :documentation "estimated rewards for each player, vector of numbers")
   ;; N
   (visit-count :type integer
                :initform 0
                :accessor visit-count
                :initarg :visit-count)
   (unexplored-actions :type list
                       :accessor unexplored-actions
                       :initarg :unexplored-actions)
   (children :type list
             :initform nil
             :accessor children
             :initarg :children
             :documentation "list of pairs (action . node) where
   'action' applied to state for 'this node' leads to 'node'") ))

(defgeneric possible-actions (state player)
  (:documentation "return the list of possible actions for specified player"))

(defgeneric next-state (state action)
  (:documentation "apply action to the state and return new state"))

(defgeneric clone-state (state)
  (:documentation "return copy of the state that could be modified locally"))

(defgeneric estimate-state-rewards (state)
  (:documentation "return vector of reward values one per each player"))

(defun get-best-move (root)
  (let* ((*exploration-coefficient* 0))
    (cdr (select-best-child root))))

(defun select-next-move (&key
                           root-state
                           root-player
                           max-iters
                           timeout-in-seconds
                           (exploration-coefficient 1)
                           (max-selection-depth 1000)
                           players-number)
  (assert (or max-iters timeout-in-seconds))
  (assert players-number)
  (assert root-state)
  (assert root-player)
  (let* ((stop-time (when timeout-in-seconds
                      (+ (get-internal-run-time)
                         (* timeout-in-seconds
                            internal-time-units-per-second))))
         (*exploration-coefficient* exploration-coefficient)
         (*max-selection-depth* max-selection-depth)
         (*players-number* players-number)
         (root (make-node-for-state root-state
                                    :parent nil
                                    :player root-player))
         (i 0))
    (loop while (and (< i max-iters)
                     (or (null stop-time)
                         (< (get-internal-run-time) stop-time)))
       do 
         (multiple-value-bind (node state)
             (selection-loop root (clone-state root-state) 0)
           (incf i)
           (backup node (estimate-state-rewards state))))
    (get-best-move root)))

(defun selection-loop (node state depth)
  (with-slots (children unexplored-actions player) node
    (cond
      ;; no moves or out of budget
      ((or (and (null children)
                (null unexplored-actions))
           (> depth *max-selection-depth*))
       (values node state))
      ;; stop the loop and pop first unexplored
      (unexplored-actions
       (expand-node node state))
      ;; select best child and repeat
      (children
       (destructuring-bind (best-child . action)
           (select-best-child node)
         (selection-loop best-child
                         (next-state state action)
                         (1+ depth))))
      (t (error "Should never reach this state")))))

(defun expand-node (node state)
  (let* ((action (pop (unexplored-actions node)))
         (child-state (next-state state action))
         (child-node (make-node-for-state child-state
                                          :parent node
                                          :player (next-player node))))
    (push (cons child-node action) (children node))
    child-node))

(defun next-player (node)
  (mod (1+ (player node))
       *players-number*))

(defun select-best-child (node)
  (assert (not (null (children node))))
  (let ((2-ln-node-visits (* 2 (log (visit-count node))))
        (player (player node)))
    (labels ((%ucb1 (child-node)
               (+ (/ (aref (simulation-rewards child-node) player)
                     (visit-count child-node))
                  (* *exploration-coefficient*
                     (sqrt (/ 2-ln-node-visits
                              (visit-count child-node)))))))
      ;; argmax loop
      (let* ((best-child/action (first (children node)))
             (best-child-score (%ucb1 (car best-child/action))))
        (dolist (child/action (rest (children node)))
          (let ((child-score (%ucb1 (car child/action))))
            (when (> child-score best-child-score)
              (setf best-child/action child/action
                    best-child-score child-score))))
        ;; return
        best-child/action))))

(defun backup (bottom-node reward-vector)
  (loop
     for node = bottom-node then (parent node)
     while node
     do
       (incf (visit-count node))
       (let ((node-reward-vector (simulation-rewards node)))
         ;; pairwise (simulation-reward node) + reward-vector
         (loop for i below (length node-reward-vector) do
              (incf (aref node-reward-vector i)
                    (aref reward-vector i))))))

(defun make-node-for-state (state &key parent player)
  (assert player)
  (make-instance
   'node
   :unexplored-actions (possible-actions state player)
   :parent parent
   :player player))

