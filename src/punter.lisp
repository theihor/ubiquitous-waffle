(defpackage :src/punter
  (:use :common-lisp :src/graph)
  (:export :punter
           :claim-edge))

(in-package :src/punter)

(defclass punter ()
  ((id :initarg :id
       :type integer
       :reader id)
   (mine->sites :initform (make-hash-table :test #'eq)
                :accessor mine->sites
                :type hash-table
                :documentation "Table of kind mine -> { site -> t }")
   
   (site->mines :initform (make-hash-table :test #'eq)
                :accessor site->mines
                :type hash-table
                :documentation "Table of kind site -> { mine -> t }")
   (graph :initarg :graph
          :accessor punter-graph
          :type array-graph
          :documentation "Graph of edges claimed by this punter")
   (score :initarg :score
          :initform 0
          :type integer
          :accessor score)))

(defmethod initialize-instance :after ((p punter) &key mines)
  (loop :for id :from 0 :to (num-nodes (punter-graph p)) :do
     (setf (gethash id (site->mines p))
           (make-hash-table :test #'eq)))
  (loop :for mine :in mines :do
     (setf (gethash mine (mine->sites p))
           (make-hash-table :test #'eq))
     (setf (gethash mine (gethash mine (site->mines p))) t)
     (setf (gethash mine (gethash mine (mine->sites p))) t)))

(defgeneric claim-edge (punter node1 node2 distance-tab))

(defmethod claim-edge ((p punter) node1 node2 distance-tab)
  (with-slots (mine->sites site->mines graph) p
    (labels ((%node-reachable? (mine node)
               (gethash node (gethash mine mine->sites)))
             (%add-node (mine node)
               (let ((score 0))
                 (unless (%node-reachable? mine node)
                   (setf (gethash node (gethash mine mine->sites)) t)
                   (setf (gethash mine (gethash node site->mines)) t)
                   (incf score (gethash (cons mine node) distance-tab))
                   (mapc-node-edges graph node
                                    (lambda (n data)
                                      (declare (ignore data))
                                      (incf score (%add-node mine n)))))
                 score)))
      (graph:add-edge graph node1 node2 t)
      (maphash (lambda (mine val)
                 (declare (ignore val))
                 (incf (score p) (%add-node mine node2)))
               (gethash node1 site->mines))
      (maphash (lambda (mine val)
                 (declare (ignore val))
                 (incf (score p) (%add-node mine node1)))
               (gethash node2 site->mines))))
  p)

(defmacro sqr (form)
  (alexandria:with-gensyms (x)
    `(let ((,x ,form))
       (* ,x ,x))))

(defun estimate-score (punter node1 node2 distance-tab)
  "Does not mute `punter'"
  (with-slots (mine->sites site->mines graph) punter
    (let ((mine->sites-aux (make-hash-table :test #'equal))
          (delta-score 0))
      (labels ((%node-reachable? (mine node)
                 (or (gethash node (gethash mine mine->sites))
                     (gethash (cons mine node) mine->sites-aux)))
               (%add-node (mine node)
                 (let ((score 0))
                   (unless (%node-reachable? mine node)
                     (setf (gethash (cons mine node) mine->sites-aux) t)
                     (incf score (sqr (gethash (cons mine node) distance-tab)))
                     (mapc-node-edges graph node
                                      (lambda (n data)
                                        (declare (ignore data))
                                        (incf score (%add-node mine n)))))
                   score)))
        ;; (graph:add-edge graph node1 node2 t)
        (maphash (lambda (mine val)
                   (declare (ignore val))
                   (incf delta-score (%add-node mine node2)))
                 (gethash node1 site->mines))
        (maphash (lambda (mine val)
                   (declare (ignore val))
                   (incf delta-score (%add-node mine node1)))
                 (gethash node2 site->mines))
        ;; (graph:remove-edge graph node1 node2)
        delta-score))))

