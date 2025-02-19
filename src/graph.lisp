(uiop:define-package :src/graph
    (:nicknames :graph)
  (:use :common-lisp :src/utils)
  (:import-from :alexandria)
  (:export #:add-edge
           #:get-edge
           #:remove-edge
           #:make-graph
           #:mapc-node-edges
           #:any-neighbour
           #:clone-graph
           #:any-node
           
           #:array-graph
           #:hash-graph
           #:graph
           #:num-edges
           #:num-nodes
           #:graph-edges
           #:mapc-all-edges
           #:get-nodes))

(in-package :src/graph)

(defclass graph ()
  ())

(defclass array-graph (graph)
  ((num-nodes :initarg :num-nodes
              :reader num-nodes)
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
(defgeneric clone-graph (graph)
  (:documentation "Make a copy of the graph"))
(defgeneric graph->dot (graph file &key edge-label node-label)
  (:documentation "Dump graph to file in dot format"))
(defgeneric any-node (graph)
  (:documentation "Return any node of the graph that has edges"))
(defgeneric mapc-all-edges (graph func)
  (:documentation "Maps all edges of graph with params (source target data)"))
(defgeneric get-nodes (graph)
  (:documentation "Returns all nodes in form of hash-table of kind node -> t"))

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

(defmethod clone-graph ((graph array-graph))
  (with-slots (num-nodes edges) graph
    (let ((arr (make-array (list num-nodes))))
      (loop :for ind :below num-nodes
         :do (setf (elt arr ind)
                   (alexandria:copy-hash-table (elt edges ind))))
      (make-instance
       'array-graph
       :num-nodes num-nodes
       :edges arr))))

;; Hash graph

(defclass hash-graph (graph)
  ((edges :initarg :edges
          :reader graph-edges)))

(defun gethash-with-create (node tab)
  (let ((res (gethash node tab)))
    (if res
        res
        (let ((new (make-hash-table :test #'equal)))
          (setf (gethash node tab) new)
          new))))

(defmethod add-edge ((graph hash-graph) node1 node2 data)
  (with-slots (edges) graph
    (let ((adj-tab1 (gethash-with-create node1 edges))
          (adj-tab2 (gethash-with-create node2 edges)))
      (setf (gethash node2 adj-tab1) data)
      (setf (gethash node1 adj-tab2) data))))

(defmethod get-edge ((graph hash-graph) node1 node2)
  (with-slots (edges) graph
    (let ((tab (gethash node1 edges)))
      (when tab
        (gethash node2 tab)))))

(defmethod remove-edge ((graph hash-graph) node1 node2)
  (with-slots (edges) graph
    (let ((adj-tab1 (gethash node1 edges))
          (adj-tab2 (gethash node2 edges)))
      (when adj-tab1
        (remhash node2 adj-tab1)
        (when (= (hash-table-count adj-tab1) 0)
          (remhash node1 edges)))
      (when adj-tab2
        (remhash node1 adj-tab2)
        (when (= (hash-table-count adj-tab2) 0)
          (remhash node2 edges))))))

(defmethod mapc-node-edges ((graph hash-graph) node func)
  (with-slots (edges) graph
    (let ((tab (gethash node edges)))
      (when tab
        (maphash func tab)))))

(defmethod mapc-all-edges ((graph hash-graph) func)
  (maphash (lambda (source targets)
             (maphash (lambda (target data)
                        (when (< source target)
                          (funcall func source target data)))
                      targets))
           (graph-edges graph)))

(defmethod any-node ((graph hash-graph))
  (with-slots (edges) graph
    (maphash (lambda (node connections)
               (declare (ignore connections))
               (return-from any-node node))
             edges))
  nil)

(defmethod make-graph ((graph-class (eql 'hash-graph)) &rest params)
  (declare (ignore params))
  (make-instance 'hash-graph
                 :edges (make-hash-table :test #'equal)))

(defmethod clone-graph ((graph hash-graph))
  (with-slots (edges) graph
    (let ((tab (make-hash-table :test #'equal)))
      (maphash (lambda (key value)
                 (setf (gethash key tab)
                       (alexandria:copy-hash-table value)))
               edges)
      (make-instance
       'hash-graph
       :edges tab))))

(defmethod get-nodes ((graph hash-graph))
  (copy-hash-table (graph-edges graph)
                   :val-copy-func (constantly t)))

(defmethod graph->dot ((graph hash-graph) file &key (edge-label nil) (node-label nil))
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format s "graph g {~%")
    (with-slots (edges) graph
      (maphash (lambda (source targets)
                 (format s "~A [label=~A];~%" source (when node-label (funcall node-label source)))
                 (maphash (lambda (target data)
                            (when (< source target)
                              (format s "~A -- ~A [label=~A];~%"
                                      source target (if edge-label
                                                        (funcall edge-label source target)
                                                        data))))
                          targets))
               edges))
    (format s "}~%")))


