(uiop:define-package :src/test/graph
    (:use :common-lisp
          :src/graph
          :lisp-unit))

(in-package :src/test/graph)

(define-test simple.1
  (let ((graph (make-graph 'array-graph
                           :num-nodes 5)))
    (add-edge graph 0 1 5)
    (assert-equal 5 (get-edge graph 0 1))
    (assert-equal 5 (get-edge graph 1 0))
    (remove-edge graph 0 1)
    (assert-equal nil (get-edge graph 0 1))
    (assert-equal nil (get-edge graph 1 0))))

(define-test simple.2
  (let ((graph (make-graph 'array-graph
                           :num-nodes 5)))
    (add-edge graph 0 1 5)
    (add-edge graph 1 0 6)
    (assert-equal 6 (get-edge graph 0 1))
    (assert-equal 6 (get-edge graph 1 0))
    (remove-edge graph 0 1)
    (assert-equal nil (get-edge graph 0 1))
    (assert-equal nil (get-edge graph 1 0))))

(define-test simple.3
  (let ((graph (make-graph 'array-graph
                           :num-nodes 5)))
    (add-edge graph 0 2 5)
    (add-edge graph 0 3 6)
    (let ((was-2 nil)
          (was-3 nil))
      (mapc-node-edges
       graph
       0
       (lambda (node2 data)
         (case node2
           (2
            (assert-false was-2)
            (assert-equal 5 data)
            (setf was-2 t))
           (3
            (assert-false was-3)
            (assert-equal 6 data)
            (setf was-3 t))
           (otherwise (assert-true nil)))))
      (assert-true was-2)
      (assert-true was-3))))
