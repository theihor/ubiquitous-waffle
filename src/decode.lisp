(uiop:define-package :src/decode
    (:use :common-lisp :anaphora :src/game-protocol)
  (:export #:parse
           #:parse-me
           #:parse-you
           #:parse-setup
           #:parse-ready
           #:parse-moves
           #:parse-move-with-state
           #:parse-stop
           #:parse-map
           #:total-parse
           #:parse-map-from-file))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/decode)

(defun parse-rivers-inner (rivers-lst)
  (mapcar
   (lambda (river-ht)
     (make-instance
      'river
      :source (gethash "source" river-ht)
      :target (gethash "target" river-ht)))
   rivers-lst))

(defun parse-sites-inner (sites-lst)
  (mapcar (lambda (x) (gethash "id" x)) sites-lst))

(defun parse-map-inner (map-ht)
  (make-instance
   'game-map
   :sites (parse-sites-inner (gethash "sites" map-ht))
   :rivers (parse-rivers-inner (gethash "rivers" map-ht))
   :mines (gethash "mines" map-ht)))

(defun parse-claim (claim-ht &optional (class-sym 'claim))
  (make-instance
   class-sym
   :punter (gethash "punter" claim-ht)
   :source (gethash "source" claim-ht)
   :target (gethash "target" claim-ht)))

(defun parse-pass (pass-ht)
  (make-instance
   'pass
   :punter (gethash "punter" pass-ht)))

(defun parse-splurge (splurge-ht)
  (make-instance
   'splurge
   :punter (gethash "punter" splurge-ht)
   :route (gethash "route" splurge-ht)))

(defun parse-move (move-ht)
  (acond
    ((gethash "claim" move-ht)
     (parse-claim it))
    ((gethash "pass" move-ht)
     (parse-pass it))
    ((gethash "splurge" move-ht)
     (parse-splurge it))
    ((gethash "option" move-ht)
     (parse-claim it 'option))
    (t (error "unknown type of move"))))

(defun parse-moves-inner (moves-ht)
  (mapcar #'parse-move (gethash "moves" moves-ht)))

(defun parse-score (score-ht)
  (make-instance
   'score-info
   :punter (gethash "punter" score-ht)
   :score (gethash "score" score-ht)))

(defun parse-stop-inner (stop-ht)
  (make-instance
   'stop
   :moves (mapcar #'parse-move (gethash "moves" stop-ht))
   :scores (mapcar #'parse-score (gethash "scores" stop-ht))))


(defun get-handshake (msg-ht)
  (gethash "you" msg-ht))

(defun get-confirmation (msg-ht)
  (gethash "me" msg-ht))

(defun setup-p (msg-ht)
  (and (gethash "punter" msg-ht)
       (gethash "punters" msg-ht)
       (gethash "map" msg-ht)))

(defun parse-settings-inner (settings-ht)
  (make-instance
   'settings
   :futures (when settings-ht
              (gethash "futures" settings-ht)
              (gethash "splurges" settings-ht)
              (gethash "optoins" settings-ht))))

(defun parse-setup-inner (setup-ht)
  (make-instance
   'setup
   :punter (gethash "punter" setup-ht)
   :punters (gethash "punters" setup-ht)
   :map (parse-map-inner (gethash "map" setup-ht))
   :settings (parse-settings-inner (gethash "settings" setup-ht))))

(defun parse-ready-inner (ready-ht)
  "Return initial state hash-table"
  (gethash "state" ready-ht))

(defun get-move (msg-ht)
  (gethash "move" msg-ht))

(defun get-stop (msg-ht)
  (gethash "stop" msg-ht))

(defun get-timeout (msg-ht)
  (gethash "timeout" msg-ht))

(defun parse-state (msg-ht)
  msg-ht)

(defun total-parse-inner (ht)
  (if (typep ht 'HASH-TABLE)
      (let ((type (gethash "__type" ht)))
        (cond
          ((string= type "HASH-TABLE")
           (alexandria:plist-hash-table
            (mapcar #'total-parse-inner (gethash "content" ht))))
          ((string= type "PAIR")
           (cons (total-parse-inner (gethash "car" ht))
                 (total-parse-inner (gethash "cdr" ht))))
          ((string= type "KEYWORD")
           (intern (gethash "value" ht) :keyword))
          ((string= type "SYM")
           (let ((str (gethash "value" ht))) 
             (when str
               (if (string= str "NIL")
                   nil
                   (intern str)))))
          (t
           (when type
             (let ((instance (allocate-instance (find-class (intern type)))))
               (dolist (slot (cl-mop:slot-names instance) instance)
                 (setf
                  (slot-value instance slot)
                  (total-parse-inner (gethash (symbol-name slot) ht)))))))))
      (if (listp ht)
          (mapcar #'total-parse-inner ht)
          (if (and (stringp ht)
                   (> (length ht) 0))
              (cond ((eq (elt ht 0) #\:) (intern (subseq ht 1) :keyword))
                    ((eq (elt ht 0) #\#) (intern (subseq ht 1)))
                    (t ht))
              ht))))

(defun parse-me (msg)
  (let ((me-ht (yason:parse msg)))
    (get-confirmation me-ht)))

(defun parse-you (msg)
  (let ((you-ht (yason:parse msg)))
    (get-handshake you-ht)))

(defun parse-setup (msg)
  (let ((setup-ht (yason:parse msg)))
    (parse-setup-inner setup-ht)))

(defun parse-ready (msg)
  (let ((ready-ht (yason:parse msg)))
    (parse-ready-inner ready-ht)))

(defun parse-moves (msg)
  (let ((move-ht (yason:parse msg)))
    (parse-moves-inner (get-move move-ht))))

(defun parse-move-with-state (msg)
  (let ((move-ht (yason:parse msg)))
    (values
     (parse-move move-ht)
     (gethash "state" move-ht))))

(defun parse-stop (msg)
  (let ((score-ht (yason:parse msg)))
    (parse-stop-inner (get-stop score-ht))))

(defun parse-map (map)
  (parse-map-inner (yason:parse map)))

(defun parse (json)
  (let ((json-ht (yason:parse json)))
    (values
     (acond
       ((get-handshake json-ht)
        it)
       ((setup-p json-ht)
        (parse-setup-inner json-ht))
       ((get-move json-ht)
        (parse-moves-inner it))
       ((get-stop json-ht)
        (parse-stop-inner it))
       ((get-timeout json-ht)
        it))
     (total-parse-inner (gethash "state" json-ht))
     json-ht)))

(defun parse-map-from-file (map-file) 
  (with-open-file (stream map-file)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      (parse-map contents))))

(defun total-parse (json)
  (let ((json-ht (yason:parse json)))
    (total-parse-inner json-ht)))
