(defpackage :src/punter
  (:use :common-lisp :src/graph))

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
   (graph :initform (make-graph 'array-graph)
          :accessor punter-graph
          :type array-graph
          :documentation "Graph of edges claimed by this punter")
   (score :initarg :score
          :initform 0
          :type integer
          :accessor score)))

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

