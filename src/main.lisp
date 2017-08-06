(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :src/decode :src/encode 
        :src/game-protocol
        :src/game-player
        :src/future-player
        :src/punter
        :src/game-state
        :src/graph
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

(defun run-players-on-port (players port)
  "Runs players from the PLAYERS list on game with port PORT"
  (dolist (player players)
    (sb-thread:make-thread
     (lambda () (main-online port player)))))

;; (defun main ()
;;   (when sb-ext:*posix-argv*
;;     (let* ((parsed-args (apply-argv:parse-argv* ;;'("./test" "-f" "problems/problem_1.json")))
;; 			 sb-ext:*posix-argv*))
;; 	   (files) (phrases) (time) (memory) (proc-count))
;;       ;;(format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
;;       (mapcar (lambda (p) 
;; 		(let ((o (string (car p)))
;; 		      (v (cdr p)))
;; 		  (cond
;; 		    ((string= "-f" o) (push v files))
;; 		    ((string= "-p" o) (push v phrases))
;; 		    ((string= "-c" o) (setq proc-count v))
;; 		    ((string= "-m" o) (setq memory v))
;; 		    ((string= "-t" o) (setq time (parse-integer v :junk-allowed t))))))
;; 	      (alexandria:plist-alist (cdr parsed-args)))
;;       (when time
;;         (set-timeout time))
;;       (setq *magic-words* phrases)
;;       (setq *magic-words-cst* (make-command-seq-matching-tree phrases))
;;       ;;(format t "~A~%" files)
;;       (let ((result-list nil))
;; 	(dolist (f (reverse files))			
;; 	  (when (probe-file f)
;; 	    ;;(format t "~A~%~%" (alexandria:read-file-into-string f))
;;             (debug-log "Processing file ~A~%" f)
;; 	    (setf result-list 
;; 		  (append result-list (let ((*standard-output* *error-output*))
;;                                         (simple-wave-from-task 
;;                                          (decode-task (alexandria:read-file-into-string f)))))))) 
;; 	(yason:encode result-list)))))
