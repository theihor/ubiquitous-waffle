(uiop:define-package :src/test/tic-tac-toe
    (:use :common-lisp
          :src/mcts))

(declaim (optimize (debug 3) (safety 3)))

(in-package :src/test/tic-tac-toe)

(defparameter *players-number* 2)

(defclass tic-tac-toe ()
  ((field :reader field
          :initarg :field)
   (winner? :accessor winner?
            :initarg :winner?
            :initform nil)))

(defun make-initial-state (size)
  (make-instance
   'tic-tac-toe
   :field (make-array `(,size ,size)
                      :element-type 'symbol
                      :initial-element :.)))

(defmethod show-state ((state tic-tac-toe))
  (with-output-to-string (s)
    (let* ((field (field state))
           (dims (array-dimensions field)))
      (format s "    ")
      (loop for j below (second dims) do
           (format s "~3d" j))
      (format s "~%")
      (loop for i below (first dims) do
           (format s "~3A: " i)
           (loop for j below (second dims) do
                (format s " ~A " (aref field i j)))
           (format s "~%")))
    (when (winner? state)
      (format s "winner: ~A~%" (winner? state)))))

(defmethod possible-actions ((s tic-tac-toe) p)
  (let* ((f (field s))
         (dims (array-dimensions f))
         (actions nil)
         (free-cells nil))
    (loop for i below (first dims) do
         (loop for j below (second dims) do
              (when (eq (aref f i j) :.)
                (push (cons i j) free-cells))
              (labels ((%push? (i j)
                         (when (free? f i j)
                           (push (list i j p) actions))))
                (unless (free? f i j)
                  (%push? (1+ i) j)
                  (%push? (1- i) j)
                  (%push? i (1+ j))
                  (%push? i (1- j))))))
    (cond
      (actions actions)
      (free-cells
       (let* ((ci (floor (first dims) 2))
              (cj (floor (second dims) 2))
              (free-cells
               (sort
                free-cells #'<
                :key (lambda (cell)
                       (destructuring-bind (i . j) cell
                         (let ((a (- i ci))
                               (b (- j cj)))
                           (+ (* a a) (* b b)))))))
              (cell (first free-cells)))
         (list (list (car cell) (cdr cell) p)))))))

(defun free? (f i j)
  (let ((r
         (and (<= 0 i)
              (<= 0 j)
              (< i (array-dimension f 0))
              (< j (array-dimension f 1))
              (eq (aref f i j) :.)
              )))
    r))

(defmethod next-state ((s tic-tac-toe) a)
  (destructuring-bind (i j p) a
    (let ((f (field s)))
      (assert (eq (aref f i j) :.))
      (setf (aref f i j) p)
      (let ((up (count-moves f i j 1 0))
            (down (count-moves f i j -1 0))
            (left (count-moves f i j 0 -1))
            (right (count-moves f i j 0 1)))
        (when (member 5 (list (+ up down 1)
                              (+ left right 1)))
          (setf (winner? s) p))))
    s))

(defun count-moves (f start-i start-j di dj)
  (let ((p (aref f start-i start-j))
        (count -1)) ;; don't count for self
    (block loops
      (loop
         for i = start-i then (+ i di)
         for j = start-j then (+ j dj)
         while (and (<= 0 i)
                    (<= 0 j)
                    (> (array-dimension f 0) i)
                    (> (array-dimension f 1) j))
         do
           (if (eq p (aref f i j))
               (incf count)
               (return-from loops))))
    count))

(defmethod clone-state ((s tic-tac-toe))
  (let* ((size (array-dimension (field s) 0))
         (c (make-initial-state size)))
    (setf (slot-value c 'winner?) (winner? c))
    (loop for i below size do
         (loop for j below size do
              (setf (aref (field c) i j)
                    (aref (field s) i j))))
    c))

(defmethod estimate-state-rewards ((s tic-tac-toe) p)
  (if (winner? s)
      (let ((r (make-array *players-number* :initial-element 0)))
        (setf (aref r (winner? s)) 1)
        r)
      (let* ((actions-list (possible-actions s p)))
        (if actions-list
            (let* ((actions (coerce actions-list 'vector))
                   (n (random (array-dimension actions 0))))
              (next-state s (aref actions n))
              (estimate-state-rewards s (mod (1+ p) *players-number*)))
            (make-array *players-number*
                        :initial-element (/ 1 *players-number*))))))

;; TODO: debug me with this:
;; (SRC/TEST/TIC-TAC-TOE::play :debug-file "~/tmp/ttt" :seed 200)
;; just put 0 up
;; probably inadequate estimation
(defun play (&key debug-file seed)
  (let ((*players-number* 2)
        (*random-state* (if seed
                            (sb-ext:seed-random-state seed)
                            *random-state*))
        (state (make-initial-state 10)))
    (loop while (or (null (winner? state))
                    (null (possible-actions state 0)))
       do
       ;; your turn
         (format t "~A" (show-state state))
         (let (i j)
           (loop do
                (format t "Your move: ")
                (setf i (read)
                      j (read))
                (if (and (integerp i) (integerp j))
                    (if (free? (field state) i j)
                        (return)
                        (format t "This cell is not free~%"))
                    (format t "Please enter two integers~%")))
           (setf state (next-state state (list i j 0)))
           (when (winner? state) (return)))
       ;; my turn
         (multiple-value-bind (move root-node)
             (select-next-move
              :root-state state
              :root-player 1
              :players-number *players-number*
              :max-iters 200
              :max-selection-depth 5)
           (when debug-file
             (let ((dot (make-pathname :defaults debug-file
                                       :type "dot")))
               (with-open-file (stream dot
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (print-decision-tree stream root-node state))
               (sb-ext:run-program "dot" (list "-Tsvg" "-O" (namestring (truename dot)))
                                   :search t
                                   :wait t
                                   :error *error-output*
                                   :output *standard-output*
                                   :if-error-exists :error)
               ;; (sb-ext:run-program "eog" (list (format nil "~A.svg" (namestring (truename dot))))
               ;;                     :search t
               ;;                     :wait nil)
               ))
           (if (null move)
               (progn
                 (format t "No moves left for me~%")
                 (return))
               (progn
                 (format t "My move: ~A~%" (butlast move))
                 (setf state (next-state state move))))))
    (when (winner? state)
      (format t "~A" (show-state state)))))
