(defpackage :src/bfs
  (:nicknames :bfs)
  (:use :common-lisp)
  (:import-from :src/graph
                :mapc-node-edges))

(in-package :bfs)

(defun bfs-distances (g root)
  "Returns hash-table of kind node -> distance"
  (let ((distances (make-hash-table :test #'eq)) 
        (steps 0)
        (nodes (list root))) 
    (labels ((%visited? (node)
               (gethash node distances))
             (%step (node)
               (let ((next-nodes nil))
                 (mapc-node-edges g node
                                  (lambda (successor data)
                                    (declare (ignore data))
                                    (unless (%visited? successor)
                                      (push successor next-nodes)
                                      (setf (gethash successor distances) steps))))
                 next-nodes)))
      (loop :while nodes :do
         (setf nodes (loop :for n :in nodes :append (%step n)))
         (incf steps))
      distances)))

(defun multiple-bfs-distances (g source-nodes)
  "Returns hash table of kind (source . target) -> distance"
  (let ((result (make-hash-table :test #'equal)))
    (loop :for source :in source-nodes :do
       (let ((distances (bfs-distances g source)))
         (maphash (lambda (target d)
                    (setf (gethash (cons source target) result) d))
                  distances)))
    result))

