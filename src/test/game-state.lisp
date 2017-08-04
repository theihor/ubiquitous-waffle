(uiop:define-package :src/test/game-state
    (:use :common-lisp
          :src/game-state
          :src/game-protocol
          :src/graph)
  (:import-from :lisp-unit))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/test/game-state)

(defmacro define-test (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (lisp-unit:define-test ,name
       (,name)
       (lisp-unit:assert-equal t t))))

(defun sample-setup-message ()
  (make-instance
   'setup
   :punter 1
   :punters 2
   :map (make-instance
         'game-map
         :sites 3
         :mines (list 0 2)
         :rivers (list (make-instance 'river
                                      :source 0
                                      :target 1)
                       (make-instance 'river
                                      :source 2
                                      :target 1)))))

(defun check-sample-state (s connections)
  (assert (= 2 (players-number s)))
  (assert (= 1 (id s)))
  (assert (equalp'(0 2) (mines s)))
  (dolist (c connections)
    (destructuring-bind (from to val) c
      (assert (equalp (get-edge (game-map s) from to) val)))))

(define-test make-state.1 ()
  (let* ((m (sample-setup-message))
         (s (make-game-state m)))
    (check-sample-state s '((0 1 :free)
                            (2 1 :free)
                            (0 2 nil)))))

(define-test process-moves.1
  (let* ((m (sample-setup-message))
         (s (make-game-state m)))
    (process-moves s (list (make-instance 'pass)))
    (check-sample-state s '((0 1 :free)
                            (2 1 :free)
                            (0 2 nil)))
    (process-moves s (list (make-instance 'claim
                                          :punter 1
                                          :source 0
                                          :target 1)))
    (check-sample-state s '((0 1 1)
                            (2 1 :free)
                            (0 2 nil)))
    ))
