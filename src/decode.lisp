(uiop:define-package :src/decode
    (:use :common-lisp :src/game-protocol)
  (:export #:parse-you
           #:parse-setup))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/decode)

(defun get-json-from-msg (msg)
  (let ((msg-len (length msg))
        (json-len (parse-integer msg :junk-allowed t)))
    (subseq msg (- msg-len json-len))))

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


(defun parse-you (msg)
  (let ((you-ht (yason:parse (get-json-from-msg msg))))
    (gethash "you" you-ht)))

(defun parse-setup (msg)
  (let ((setup-ht (yason:parse (get-json-from-msg msg))))
    (make-instance
     'setup
     :punter (gethash "punter" setup-ht)
     :punters (gethash "punters" setup-ht)
     :map (parse-map-inner (gethash "map" setup-ht)))))
