(uiop:define-package :src/simulator
    (:use :common-lisp :anaphora)
  (:export #:get-random-map-json
           ))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/simulator)

(defvar *base-url* "http://punter.inf.ed.ac.uk")

(defun get-random-map-json ()
  "Return random map from punter server."
  (let* ((raw-html
          (drakma:http-request (format nil "~a/~a" *base-url* "status.html")))
         (maplist
          (remove-duplicates
           (cl-ppcre:all-matches-as-strings
            "/maps/[a-zA-Z0-9-]+.json" raw-html)
           :test #'string=))
         (random-map (elt maplist (random (length maplist)))))
    (flexi-streams:octets-to-string
     (drakma:http-request (format nil "~a~a" *base-url* random-map)))))
