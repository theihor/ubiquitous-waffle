(uiop:define-package :src/test/game-player
    (:use :common-lisp
          :src/game-state
          :src/game-player
          :src/game-protocol
          :src/graph)
  (:import-from :lisp-unit
                :assert-true
                :assert-equal)
  (:import-from :src/test/game-state
                :define-test))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/test/game-player)

(defun sample-setup-message ()
  (make-instance
   'setup
   :punter 0
   :punters 1
   :map (make-instance
         'game-map
         :sites '(0 1 2)
         :mines (list 0)
         :rivers (list (make-instance 'river
                                      :source 0
                                      :target 1)
                       (make-instance 'river
                                      :source 2
                                      :target 1)))))

(defun check-move (player func)
  (let ((move (select-move player)))
    (funcall func move)
    (update-player player (list move))))

(define-test cowboy.1
  (let ((player (make-player 'cowboy-player)))
    (init-player player (sample-setup-message))
    (check-move
     player
     (lambda (move)
       (assert-true (typep move 'claim))
       (assert-equal 0 (move-punter move))
       (assert-equal 0 (claim-source move))
       (assert-equal 1 (claim-target move))))
    (check-move
     player
     (lambda (move)
       (assert-true (typep move 'claim))
       (assert-equal 0 (move-punter move))
       (assert-equal 1 (claim-source move))
       (assert-equal 2 (claim-target move))))
    (check-move
     player
     (lambda (move)
       (assert-true (typep move 'pass))
       (assert-equal 0 (move-punter move))))))
