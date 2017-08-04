(uiop:define-package :src/test/game-state
    (:use :common-lisp
          :src/game-state
          :src/game-protocol
          :src/graph
          :src/decode)
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
         :sites '(0 1 2)
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

(defparameter *simple-game* "{\"punter\":0,
\"punters\":2,
\"map\":{\"sites\":[{\"id\":4},{\"id\":1},{\"id\":3},{\"id\":6},{\"id\":5},{\"id\":0},{\"id\":7},{\"id\":2}],
\"rivers\":[{\"source\":3,\"target\":4},{\"source\":0,\"target\":1},{\"source\":2,\"target\":3},{\"source\":1,\"target\":3},{\"source\":5,\"target\":6},{\"source\":4,\"target\":5},{\"source\":3,\"target\":5},{\"source\":6,\"target\":7},{\"source\":5,\"target\":7},{\"source\":1,\"target\":7},{\"source\":0,\"target\":7},{\"source\":1,\"target\":2}],\"mines\":[1,5]}}")

(define-test build-map-from-json-input
  (let ((setup (parse *simple-game*)))
    (assert (eq (setup-punter setup) 0))
    (assert (eq (setup-punters setup) 2))
    (assert (not (null (setup-map setup))))))
