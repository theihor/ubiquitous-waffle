(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :src/decode :src/encode 
        :src/game-protocol
        :src/game-player
        :src/punter
        :src/game-state
        :src/graph))

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
      (host-not-found-error ()
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

;;run example (main-online 9066 'cowboy-player)
(defun main-online (port &rest player-params)
  (let ((socket (tcp-connect *punter-server* port))
	(player (apply #'make-player player-params))
	(s))
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
	   (setf s (parse s))
	   (when (and s (typep s 'setup))
	     (format t "Getting setup... Punter:~A~%" (setup-punter s))
	     (init-player player s)

	     ;;TODO: Add futures handling

	     ;; send ready
	     (format t "Sending ready...~%")
	     (tcp-send socket (encode-ready (setup-punter s)))
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
			(let ((new-move (select-move player)))
			  (tcp-send socket (encode-move new-move))
              (setf (move-state new-move) player)
              (format t "Encoded move: ~A~%" (encode-move new-move)))))
		    (t (format t "Timeout.~%")))))))
      ;; (dump-state (state player) "~/g.dot")
      (progn (format t "~&Closing listen socket~%")
	     (sb-bsd-sockets:socket-close socket)))))

(defun format-std (str &rest params)
  (apply #'format *standard-output* str params)
  (finish-output *standard-output*))

;; via stdin, stdout, stderr
(defun main-offline ()

  (let ((stdin *standard-input*)
        (stdout *standard-output*)
        (player (make-player 'connector-player))
        (*package* (find-package :src/main)))
    
    (format *error-output* "Sending me...~%")
    
    (format-std "~A" (encode-me "SpiritRaccoons"))
    (format *error-output* "Getting you... ~A~%" (read-with-size stdin))
    
    (let ((msg (read-with-size stdin)))
      (format *error-output* "From server: ~A~%" msg)
      (multiple-value-bind (m state) (parse msg)
        (cond 
          ((typep m 'setup)
           (format *error-output* "Init new player.~%")
           (init-player player m)
           (format *error-output* "Sending ready...~%")
           ;; TODO: Futures should be in ready
           (format-std "~A" (encode-ready (setup-punter m) :state player)))
          ((typep m 'stop)
           (progn
             (format *error-output* "Game stop.~%")
             (when (typep (state player) 'game-with-scores)
               (format *error-output* "Computed score:~%")
               (loop :for punter :below (players-number (state player))
                  :do (format *error-output* "~A :~A~%"
                              punter
                              (score (elt (punters (state player)) punter)))))))
          ((typep (car m) 'move)
           (format *error-output* "Getting new moves...~%")
           (when state
             (setf player state)
             (update-player player m)
             (let* ((new-move (select-move player))
                    (dummy (setf (move-state new-move) player))
                    (encoded-move (encode-move new-move)))
               (format *error-output* "Sending move... ~A~%" encoded-move)
               (format-std "~A" encoded-move)))
           )
          (t (format *error-output* "Timeout.~%")))))
    )
  )

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
;;             (format *error-output* "Processing file ~A~%" f)
;; 	    (setf result-list 
;; 		  (append result-list (let ((*standard-output* *error-output*))
;;                                         (simple-wave-from-task 
;;                                          (decode-task (alexandria:read-file-into-string f)))))))) 
;; 	(yason:encode result-list)))))
