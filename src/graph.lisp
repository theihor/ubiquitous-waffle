(uiop:define-package :src/graph
    (:nicknames :graph)
  (:use :common-lisp)
  (:export #:add-edge
           #:get-edge
           #:remove-edge
           #:make-graph
           #:mapc-node-edges
           #:any-neighbour
           
           #:array-graph
           #:num-edges))

(in-package :src/graph)

(defclass array-graph ()
  ((num-nodes :initarg :num-nodes)
   (edges :initarg :edges)))

(defgeneric add-edge (graph node1 node2 data)
  (:documentation "Add edge from node1 to node2 with data"))
(defgeneric get-edge (graph node1 node2)
  (:documentation "Get data for the edge from node1 to node2"))
(defgeneric remove-edge (graph node1 node2)
  (:documentation "Remove edge from node1 to node2"))
(defgeneric make-graph (graph-class &rest params)
  (:documentation "Make graph of class graph-class with params"))
(defgeneric mapc-node-edges (graph node func)
  (:documentation "Map func with params (node edge-data) to all edges coming from node"))

(defun check-nodes (graph &rest node-nums)
  (with-slots (num-nodes) graph
    (loop :for node :in node-nums
       :do (assert (and (>= node 0) (< node num-nodes))))))

(defmethod add-edge ((graph array-graph) node1 node2 data)
  (with-slots (num-nodes edges) graph
    (check-nodes graph node1 node2)
    (let ((adj-tab1 (elt edges node1))
          (adj-tab2 (elt edges node2)))
      (setf (gethash node2 adj-tab1) data)
      (setf (gethash node1 adj-tab2) data))))

(defmethod get-edge ((graph array-graph) node1 node2)
  (with-slots (num-nodes edges) graph
    (check-nodes graph node1 node2)
    (gethash node2 (elt edges node1))))

(defmethod remove-edge ((graph array-graph) node1 node2)
  (with-slots (num-nodes edges) graph
    (check-nodes graph node1 node2)
    (let ((adj-tab1 (elt edges node1))
          (adj-tab2 (elt edges node2)))
      (remhash node2 adj-tab1)
      (remhash node1 adj-tab2))))

(defmethod mapc-node-edges ((graph array-graph) node func)
  (with-slots (num-nodes edges) graph
    (check-nodes graph node)
    (maphash func (elt edges node))))

(defmethod make-graph ((graph-class (eql 'array-graph)) &key num-nodes)
  (let ((arr (make-array (list num-nodes))))
    (loop :for ind :below num-nodes
       :do (setf (elt arr ind)
                 (make-hash-table :test #'equal)))
    (make-instance 'array-graph
                   :num-nodes num-nodes
                   :edges arr)))

(defun any-neighbour (graph node)
  "Returns two value - node index and data for some neighbour of node.
Nil when there is no neighbours."
  (mapc-node-edges
   graph node
   (lambda (neighbour data)
     (return-from any-neighbour (values neighbour data))))
  nil)
