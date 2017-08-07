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
        :src/mcts-player)
  (:import-from :sb-sprof))

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
                       :initform nil)
   (game-log-futures :accessor game-log-futures
                     :initform nil)
   (game-log-scores :accessor game-log-scores
                    :initform nil)
   (game-log-punter :accessor game-log-punter
                    :initform nil)))

(defvar *game-log*)

(defun game-logger-setup (s futures)
  (setf (game-log-map *game-log*) (gethash "map" s))
  (setf (game-log-futures *game-log*) futures))

(defun game-logger-add-move (move)
  (push move (game-log-rev-moves *game-log*)))

(defun game-logger-add-scores (scores)
  (setf (game-log-scores *game-log*) scores))

(defun game-logger-set-punter (punter)
  (setf (game-log-punter *game-log*) punter))

(defun game-logger-print (tag)
  (let ((*yason-lisp-readable-encode* nil)
        (path (make-pathname :directory (list :relative "log") :name tag :type "json")))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (yason:with-output (stream)
        (yason:with-object ()
          (yason:encode-object-element "map" (game-log-map *game-log*))
          (yason:encode-object-element "moves" (reverse (game-log-rev-moves *game-log*)))
	  (when (game-log-futures *game-log*)
	    (yason:encode-object-element "futures" (game-log-futures *game-log*)))
          (yason:encode-object-element "scores" (game-log-scores *game-log*))
          (yason:encode-object-element "punter" (game-log-punter *game-log*)))))))

(defparameter *verbose* t)

;;run example (main-online 9066 'cowboy-player)
(defun main-online (port &rest player-params)
  (let ((socket (tcp-connect *punter-server* port))
	(player (apply #'make-player player-params))
        (*game-log* (make-instance 'game-log))
	(s)
        (setup-ht)
        (the-state)
        (team-name "SpiritRaccoons"))
    (unwind-protect
	 (progn
	   ;; send me
	   (format t "Sending me...~%")
	   (tcp-send socket (encode-me team-name))
	   ;; get you
	   (format t "Getting you... ~A~%" (tcp-read socket))
	   ;; get setup
	   (setf s (tcp-read socket))
	   ;; (format t "~A~%" s)
	   (multiple-value-setq (s the-state setup-ht) (parse s))
	   (when (and s (typep s 'setup))
	     (format t "Getting setup... Punter:~A~%" (setup-punter s))
             (game-logger-set-punter (setup-punter s))
	     (init-player player s)

	     ;; send ready
             (format t "Sending ready...~%")
             (when *verbose*
	       (format t "Bid on futures:")
	       (mapcar #'yason:encode (player-futures player))
	       (format t "~%"))
	     (tcp-send socket
		       (encode-ready (setup-punter s)
				     :futures (player-futures player)))
	     (game-logger-setup setup-ht (player-futures player))
	     ;; loop for moves until stop
	     (loop
		;; get move
		(let* ((move-or-stop-or-timeout (tcp-read socket))
		       (m (parse move-or-stop-or-timeout)))
		  (when *verbose* (format t "~A~%" move-or-stop-or-timeout))
		  (cond
                    ((numberp m)
                     ;;(debug-log "Failed with timeout ~A~%" m)
                     t)
		    ((typep m 'stop)
                     (format t "stop msg = ~A~%" move-or-stop-or-timeout)
                     (game-logger-add-scores (stop-scores m))
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
             (game-logger-print
              (format nil "~A-~A-~A-~A"
                      team-name
                      (get-player-name player)
                      port
                      (get-internal-real-time)))))))

(defun format-std (str &rest params)
  (apply #'format *standard-output* str params)
  (finish-output *standard-output*))

(defparameter *do-logging* nil)
(defparameter *logger-name* "punter")

(defun debug-log (str &rest params)
  (when *do-logging*
    (format *error-output* "~A: " *logger-name*)
    (apply #'format *error-output* str params)))

(defmacro with-profiling (&body body)
  `(progn (sb-sprof:reset)
          (sb-sprof:start-profiling :max-samples 1000 :mode :time :sample-interval 0.001)
          (progn ,@body)
          (sb-sprof:stop-profiling)
          (cl-user::sbcl-print-callgraph-in-dot-format "~/profile.dot")
          ;; (sb-sprof:report :min-percent 1 :type :flat :stream *error-output*)
          ))

;; via stdin, stdout, stderr
(defun main-offline ()
  (handler-case
      (let ((stdin *standard-input*)
            ;; (stdout *standard-output*)
            (player (make-player 'connector-player :gambling t :tricky nil :use-options t))
            (*package* (find-package :src/main)))
     
        (debug-log "Sending me...~%")
     
        (format-std "~A" (encode-me "SpiritRaccoons"))
        (debug-log "Sent me~%")
        (debug-log "Getting you... ~A~%" (read-with-size stdin))
        (let ((msg (read-with-size stdin)))
          ;; (debug-log "From server: ~A~%" msg)
          (multiple-value-bind (m state) (parse msg)
            (debug-log "m: ~A~%" m)
            (cond
              ((numberp m)
               (debug-log "Failed with timeout ~A~%" m)
               t)
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
              ((or (null m)
                   (typep (car m) 'move))
               (debug-log "Getting new moves...~%")
               (when state
                 (setf player state)
                 (update-player player m)
                 (debug-log "Player updated...~%")
                 (let* ((new-move (awhen (select-move player)
                                         (debug-log "Selected move ~A~%" it)
                                         it))
                        (dummy (setf (move-state new-move) player))
                        (dummy2 (debug-log "Move selected...~%"))
                        (encoded-move (encode-move new-move)))
                   (declare (ignorable dummy dummy2))
                   (debug-log "Sending move...~%")
                   (format-std "~A" encoded-move)))
               )
              (t (debug-log "Timeout.~%")))))
        )
    (error (e) (debug-log "Fail with error: ~A" e))))

(defun run-players-on-port (port &rest players)
  "Runs players from the PLAYERS list on game with port PORT"
  (dolist (player players)
    (sb-thread:make-thread
     (lambda ()
       (if (listp player)
           (apply #'main-online port player)
           (main-online port player))))))

(defclass bot ()
  ((id :accessor id
       :initform nil)
   (name :accessor name)
   (process :accessor process
            :initform nil)
   (setupped :accessor setupped)
   (program-name :accessor program-name
                 :initarg :program-name)))

(defun restart-bot (bot)
  (awhen (process bot)
         (close (sb-ext:process-input it))
         (close (sb-ext:process-output it))
         (sb-ext:process-kill it 9))
  (setf (process bot)
        (sb-ext:run-program
         (program-name bot) nil :output :stream :input :stream
         :error *error-output* :wait nil)))

(defun read-from-bot (bot)
  (read-with-size (sb-ext:process-output (process bot))))

(defun write-to-bot (bot str &rest args)
  (let ((*standard-output* (sb-ext:process-input (process bot))))
    (apply #'format-std str args)))

(defun main-simulator-aux (botlist &key map-file)
  (let* ((*logger-name* "simulator")
         (*random-state* (make-random-state t))
         (botlist (mapcar (lambda (name)
                            (make-instance 'bot :program-name name))
                          botlist))
         (botloop (copy-list botlist))
         (game-map (if map-file
                       (parse-map-from-file map-file)
                       (parse-map (get-random-map-json))))
         (rivers (length (map-rivers game-map)))

         (player-table (make-hash-table :test #'equal)) 
         (scores-table (make-hash-table :test #'equal))

         (moves)
         (steps 0)
         (new-id 0)) 
    
    (setf (cdr (last botloop)) botloop)

    (debug-log "rivers: ~A~%" rivers)
    
    (loop :for bot :in botloop
       ;; :for steps := 0 :then (1+ steps)
       :while (< steps rivers) :do
       (let* ()
         (sb-ext:gc :full t)
         (restart-bot bot)
         (unless (id bot)
           (setf (id bot) (incf new-id)))

         (debug-log "step: ~A~%" steps)
         
         ;; run bot with in/out piping
         (debug-log "Running bot ~A: ~a~%" (id bot) (program-name bot))
         
         ;; perfom handshake
         (debug-log "Reading message~%")
         (let ((name (parse-me (read-from-bot bot)))
               (id (id bot)))
           (setf (name bot) name)
           (debug-log "Shook hands with ~a~%" name)
           (write-to-bot bot "~A" (encode-you id))
           ;; setup or continue
           (handler-case
               (if (gethash id player-table)
                   (progn 
                     (let* ((*yason-lisp-readable-encode* nil)
                            (*trace-output* *error-output*)
                            (encoded-moves
                             (encode-moves moves (gethash id player-table)))
                            (response 0))
                       (trivial-timeout:with-timeout (1)
                         (time
                          (progn
                            ;; send current moves and state
                            (write-to-bot bot encoded-moves)
                            (setf response (read-from-bot bot)))))
                       ;; get next move
                       (multiple-value-bind (move state)
                           (parse-move-with-state response)
                         (setf (gethash id player-table) state)
                         (push move moves)))
                     (incf steps))
                   (progn
                     (debug-log "Performing setup...~%")
                     (write-to-bot bot
                                   (encode-setup
                                    (make-setup id (length botlist) game-map
                                                (make-instance 'settings
                                                               :futures t
                                                               :splurges t
                                                               :options t))
                                    ))
                     ;; (debug-log "Sent: ~A~%" (encode-setup (make-setup id (length botlist) game-map)))
                     (setf (gethash id player-table)
                           (parse-ready (read-from-bot bot)))
                     (setf (setupped bot) t)))          
             (error (e) (debug-log "ERROR: ~A~%" e)))
           ))
       :finally
       (dolist (bot botlist)
         (restart-bot bot)
         ;; perform handshake
         (let ((name (parse-me (read-from-bot bot)))
               (id (id bot)))
           (debug-log "Getting ID... ~a ~A~%" name id)
           (write-to-bot bot "~A" (encode-you id))
           ;; send stop message
           (debug-log "Sending stop to ~a...~%" id))
         (write-to-bot bot (encode-stop moves scores-table))))))

(defun main-simulator ()
  (when sb-ext:*posix-argv*
    (let* ((args (apply-argv:parse-argv (cdr sb-ext:*posix-argv*)))
           (programs (first args)))
      (main-simulator-aux programs
                          :map-file (awhen (getf (cdr args) :map-file) it))
      )))

