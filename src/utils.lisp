(uiop:define-package :src/utils
    (:use :common-lisp)
  (:export
   #:copy-instance
   #:copy-hash-table))

(in-package :src/utils)

(defun copy-instance (object &rest initargs)
  (let ((copy (allocate-instance (class-of object))))
    (dolist (slot (sb-mop:class-slots (class-of object)))
      (let ((slot-name (sb-mop:slot-definition-name slot)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name)))))
    (apply #'reinitialize-instance copy initargs)))

(defun copy-hash-table (ht &key (val-copy-func nil))
  (let ((new (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash k new)
                     (if val-copy-func
                         (funcall val-copy-func v)
                         v)))
             ht)
    new))

#+sbcl(defun cl-user::sbcl-print-callgraph-in-dot-format (file &key (min-percent 0.5))
        "Pring profile information in the form of the graph to the specified file.
Ignore functions with percent lower or equal to min-percent."
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((call-graph (sb-sprof::make-call-graph most-positive-fixnum)))
      (labels ((%accept? (node)
                 (> (sb-sprof::samples-percent call-graph (sb-sprof::node-accrued-count node))
                    min-percent)))
        (format stream "digraph CallGraph {~%~%")
        (format stream "  node [shape=box, style=filled, fillcolor=\"white\"];~%~%")
        (sb-sprof::do-vertices (node call-graph)
          (when (%accept? node)
            (let ((accrued-percent (sb-sprof::samples-percent call-graph (sb-sprof::node-accrued-count node))))
              (format stream "  ~A [label=\"~,2f / ~,2f ~A\" ~A];~%"
                      (sb-sprof::node-index node)
                      accrued-percent
                      (sb-sprof::samples-percent call-graph (sb-sprof::node-count node))
                      (sb-sprof::node-name node)
                      (cond
                        ((> accrued-percent 90) ", fillcolor=\"/ylgn8/5\"")
                        ((> accrued-percent 70) ", fillcolor=\"/ylgn8/4\"")
                        ((> accrued-percent 50) ", fillcolor=\"/ylgn8/3\"")
                        ((> accrued-percent 30) ", fillcolor=\"/ylgn8/2\"")
                        ((> accrued-percent 10) ", fillcolor=\"/ylgn8/1\"")
                        (t ""))))))
        (sb-sprof::do-vertices (from-node call-graph)
          (when (%accept? from-node)
            (let ((color-num 0))
              (sb-sprof::do-edges (call to-node from-node)
                (when (%accept? to-node)
                  (format stream "  ~A -> ~A [label=\"~,2f\" ~A];~%"
                          (sb-sprof::node-index from-node)
                          (sb-sprof::node-index to-node)
                          (sb-sprof::samples-percent call-graph (sb-sprof::call-count call))
                          (format nil ", color=\"~A\"" (case color-num
                                                         (0 "black")
                                                         (1 "red")
                                                         (2 "blue"))))
                  (setf color-num (mod (1+ color-num) 3)))))))
        (format stream "}~%")))))
