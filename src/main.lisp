(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :anaphora :src/decode :src/encode
        :src/game-protocol
        :src/game-player
        :src/future-player
        :src/punter
        :src/game-state
        :src/graph
        :src/simulator
        :src/mcts-player))

(in-package :src/main)

(require 'sb-bsd-sockets)

;; tcp
(defparameter *punter-server* "punter.inf.ed.ac.uk")

(defun nslookup (hostname)
   (and hostname
       (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))))

(defun tcp-connect (server port)
  (when (and server port)
    (handler-case
	(let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type  :stream :protocol :tcp)))
	  (sb-bsd-sockets:socket-connect socket (nslookup server) port)
	  socket)
      (sb-bsd-sockets:host-not-found-error ()
	(format t "Host ~A not found." server)
	(force-output)
	nil))))

(defun tcp-send (socket data)
   (when (and socket data)
     (sb-bsd-sockets:socket-send socket data (length data) :external-format :utf-8)))

(defun read-with-size (stream &optional (size "") (state 0))
  (case state
    (0 (let ((c (read-char stream)))
	 (if (char= c #\:)
	     (read-with-size stream (parse-integer size) 1)
	     (read-with-size stream (concatenate 'string size (string c)) 0))))
    (1 (let* ((str (make-string size)))
	 (read-sequence str stream)
	 str))))

(defun tcp-read (socket)
   (when socket
     (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :external-format :utf-8)))
       (read-with-size stream))))

(defclass game-log ()
  ((game-log-map :accessor game-log-map
                   :initform nil)
   (game-log-rev-moves :accessor game-log-rev-moves
                       :initform nil)))

(defvar *game-log*)

(defun game-logger-setup (s)
  (setf (game-log-map *game-log*) (gethash "map" s)))

(defun game-logger-add-move (move)
  (push move (game-log-rev-moves *game-log*)))

(defun game-logger-print (file-name)
  (let ((*yason-lisp-readable-encode* nil))
    (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (yason:with-output (stream)
        (yason:with-object ()
          (yason:encode-object-element "map" (game-log-map *game-log*))
          (yason:encode-object-element "moves" (reverse (game-log-rev-moves *game-log*))))))))

;;run example (main-online 9066 'cowboy-player)
(defun main-online (port &rest player-params)
  (let ((socket (tcp-connect *punter-server* port))
	(player (apply #'make-player player-params))
        (*game-log* (make-instance 'game-log))
	(s)
        (setup-ht)
        (the-state))
    (unwind-protect
	 (progn
	   ;; send me
	   (format t "Sending me...~%")
	   (tcp-send socket (encode-me "SpiritRaccoons"))
	   ;; get you
	   (format t "Getting you... ~A~%" (tcp-read socket))
	   ;; get setup
	   (setf s (tcp-read socket))
	   ;; (format t "~A~%" s)
	   (multiple-value-setq (s the-state setup-ht) (parse s))
	   (when (and s (typep s 'setup))
	     (format t "Getting setup... Punter:~A~%" (setup-punter s))
             (game-logger-setup setup-ht)
	     (init-player player s)

	     ;; send ready
	     (format t "Sending ready...~%")
	     (tcp-send
              socket
              (encode-ready (setup-punter s)
                            :futures (bid-on-futures player s)))
	     ;; loop for moves until stop
	     (loop
		;; get move
		(let* ((move-or-stop-or-timeout (tcp-read socket))
		       (m (parse move-or-stop-or-timeout)))
		  (format t "~A~%" move-or-stop-or-timeout)
		  (cond
		    ((typep m 'stop)
                     (progn
                       (format t "Game stop.~%")
                       (when (typep (state player) 'game-with-scores)
                         (format t "Computed score:~%")
                         (loop :for punter :below (players-number (state player))
                            :do (format t "~A :~A~%"
                                        punter
                                        (score (elt (punters (state player)) punter)))))
                       (return)))
		    ((typep (car m) 'move)
		     (progn
                       (update-player player m)
                       (loop :for move :in m
                          :do (game-logger-add-move move))
                       (let ((new-move (select-move player)))
                         (tcp-send socket (encode-move new-move))
                         ;; (setf (move-state new-move) player)
                         ;; (format t "Encoded move: ~A~%" (encode-move new-move))
                         )))
		    (t (format t "Timeout.~%")))))))
      ;; (dump-state (state player) "~/g.dot")
      (progn (format t "~&Closing listen socket~%")
	     (sb-bsd-sockets:socket-close socket)
             (game-logger-print "game.json")))))

(defun format-std (str &rest params)
  (apply #'format *standard-output* str params)
  (finish-output *standard-output*))

(defparameter *do-logging* nil)

(defun debug-log (str &rest params)
  (when *do-logging*
    (format *error-output* str params)))

;; via stdin, stdout, stderr
(defun main-offline ()

  (let ((stdin *standard-input*)
        ;; (stdout *standard-output*)
        (player (make-player 'connector-player))
        (*package* (find-package :src/main)))
    
    (debug-log "Sending me...~%")
    
    (format-std "~A" (encode-me "SpiritRaccoons"))
    (debug-log "Getting you... ~A~%" (read-with-size stdin))
    
    (let ((msg (read-with-size stdin)))
      (debug-log "From server: ~A~%" msg)
      (multiple-value-bind (m state) (parse msg)
        (cond 
          ((typep m 'setup)
           (debug-log "Init new player.~%")
           (init-player player m)
           (debug-log "Sending ready...~%")
           ;; TODO: Futures should be in ready
           (format-std "~A" (encode-ready (setup-punter m) :state player)))
          ((typep m 'stop)
           (progn
             (debug-log "Game stop.~%")
             (when (typep (state player) 'game-with-scores)
               (debug-log "Computed score:~%")
               (loop :for punter :below (players-number (state player))
                  :do (debug-log "~A :~A~%"
                              punter
                              (score (elt (punters (state player)) punter)))))))
          ((typep (car m) 'move)
           (debug-log "Getting new moves...~%")
           (when state
             (setf player state)
             (update-player player m)
             (debug-log "Player updated...~%")
             (let* ((new-move (select-move player))
                    (dummy (setf (move-state new-move) player))
                    (dummy2 (debug-log "Move selected...~%"))
                    (encoded-move (encode-move new-move)))
               (declare (ignorable dummy dummy2))
               (debug-log "Sending move... ~A~%" encoded-move)
               (format-std "~A" encoded-move)))
           )
          (t (debug-log "Timeout.~%")))))
    )
  )

(defun main-simulator ()
  (when sb-ext:*posix-argv*
    (let* ((output *standard-output*)
           (input *standard-input*)
           (botlist (cdr (apply-argv:parse-argv* sb-ext:*posix-argv*)))
           (botloop (copy-list botlist))
           (game-map (parse-map (get-random-map-json)))
           (rivers (length (map-rivers game-map)))

           (player-table (make-hash-table :test #'equal))
           (scores-table (make-hash-table :test #'equal))

           (moves)
           (steps 0))
      (setf (cdr (last botloop)) botloop)
      (loop :for bot :in botloop
         ;; :for steps := 0 :then (1+ steps)
         :while (< steps rivers) :do
         (progn
           ;; run bot with in/out piping
           (debug-log "Running bot ~a~%" bot)
           (sb-ext:run-program bot nil :output input :input output)
           ;; perfom handshake
           (debug-log "Reading message~%")
           (let ((id (parse-me (read-with-size input))))
             (debug-log "Shook hands with ~a~%" id)
             (format-std "~A" (encode-you id))
             ;; setup or continue
             (if (gethash id player-table)
                 (progn
                   ;; send current moves and state
                   (format-std (encode-moves moves (gethash id player-table)))
                   ;; get next move
                   (multiple-value-bind (move state)
                       (parse-move-with-state (read-with-size input))
                     (setf (gethash id player-table) state)
                     (push move moves))
                   (incf steps))
                 (progn
                   (debug-log "Performing setup...~%")
                   (format-std
                    (encode-setup (make-setup id (length botlist) game-map)))
                   (setf (gethash player-table id)
                         (parse-ready (read-with-size input)))))))
         :finally
         (dolist (bot botlist)
           ;; open in/out pipes
           (sb-ext:run-program bot nil :output input :input output)
           ;; perform handshake
           (let ((id (parse-me (read-with-size input))))
             (debug-log "Getting ID... ~a~%" id)
             (format-std "~A" (encode-you id)))
           ;; send stop message
           (debug-log "Sending stop to ~a...~%" bot)
           (format-std (encode-stop moves scores-table)))))))
