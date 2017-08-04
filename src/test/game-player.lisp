(uiop:define-package :src/test/game-player
    (:use :common-lisp
          :src/game-state
          :src/game-player
          :src/game-protocol
          :src/punter
          :src/graph
          :src/decode
          :src/encode)
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


(defun play-map (path-to-json &rest player-params)
  (let ((player (apply #'make-player player-params)))
    (labels ((%run ()
               (let ((move (select-move player)))
                 (format t "Move : ~A~%" (encode-move move))
                 (format t "Score: ~A~%" (score (elt (punters (state player)) 0)))
                 (if (typep move 'pass)
                     nil
                     (progn
                       (update-player player (list move))
                       (%run))))))
      (init-player player (make-instance 'setup
                                         :punter 0
                                         :punters 1
                                         :map (parse-map (alexandria:read-file-into-string path-to-json))))
      (%run))))
