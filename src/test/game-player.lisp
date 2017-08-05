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

(defun sample-setup-message-2 ()
  (make-instance
   'setup
   :punter 0
   :punters 1
   :map (make-instance
         'game-map
         :sites '(0 1 2 3 4 5)
         :mines (list 1 0)
         :rivers (list (make-instance 'river
                                      :source 0
                                      :target 1)
                       (make-instance 'river
                                      :source 2
                                      :target 1)
                       (make-instance 'river
                                      :source 2
                                      :target 3)
                       (make-instance 'river
                                      :source 4
                                      :target 5)))))

(defun sample-setup-message-3 ()
  (make-instance
   'setup
   :punter 0
   :punters 1
   :map (make-instance
         'game-map
         :sites '(0 1 2 3 4 5 6 7)
         :mines (list 1 5)
         :rivers (list (make-instance 'river :source 0 :target 1)
                       (make-instance 'river :source 1 :target 2)
                       (make-instance 'river :source 2 :target 3)
                       (make-instance 'river :source 3 :target 4)
                       (make-instance 'river :source 4 :target 5)
                       (make-instance 'river :source 5 :target 6)
                       (make-instance 'river :source 6 :target 7)
                       (make-instance 'river :source 7 :target 0)

                       (make-instance 'river :source 1 :target 7)
                       (make-instance 'river :source 7 :target 5)
                       (make-instance 'river :source 1 :target 3)
                       (make-instance 'river :source 3 :target 5)))))

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

(defun check-claim-0 (player src trgt)
  (check-move
   player
   (lambda (move)
     (assert-true (typep move 'claim))
     (assert-equal 0 (move-punter move))
     (assert-equal src (claim-source move))
     (assert-equal trgt (claim-target move)))))

(defun check-claim-0-unordered (player src trgt)
  (check-move
   player
   (lambda (move)
     (assert-true (typep move 'claim))
     (assert-equal 0 (move-punter move))
     (let ((res (or (and (= src (claim-source move))
                         (= trgt (claim-target move)))
                    (and (= trgt (claim-source move))
                         (= src (claim-target move))))))
       (assert-true res)))))

(defun check-pass-0 (player)
  (check-move
   player
   (lambda (move)
     (assert-true (typep move 'pass))
     (assert-equal 0 (move-punter move)))))

(define-test connector.1
  (let ((player (make-player 'connector-player)))
    (init-player player (sample-setup-message-2))
    (check-claim-0 player 1 0)
    (check-claim-0 player 1 2)
    (check-claim-0 player 2 3)
    (check-claim-0-unordered player 4 5)))

(defun play-map (path-to-json &rest player-params)
  (let ((player (apply #'make-player player-params)))
    (labels ((%run ()
               (let ((move (select-move player)))
                 (when (typep (state player) 'game-with-scores)
                     (format t "Score: ~A~%" (score (elt (punters (state player)) 0))))
                 (format t "Move : ~A~%" (encode-move move))
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

(defun play-map-setup (setup &rest player-params)
  (let ((player (apply #'make-player player-params)))
    (labels ((%run ()
               (let ((move (select-move player)))
                 (format t "Score: ~A~%" (score (elt (punters (state player)) 0)))
                 (format t "Move : ~A~%" (encode-move move))
                 (if (typep move 'pass)
                     nil
                     (progn
                       (update-player player (list move))
                       (%run))))))
      (init-player player setup)
      (%run))))
