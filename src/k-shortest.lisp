(uiop:define-package :src/k-shortest
    (:use :common-lisp
          :src/graph)
  (:import-from :cl-heap)
  (:export #:k-shortest))

(in-package :src/k-shortest)

;; Taken from https://en.wikipedia.org/wiki/K_shortest_path_routing
(defun k-shortest (g num-nodes k s t_)
  (let (;; shortest path queue
        (B (make-instance 'cl-heap:priority-queue))
        ;; set of shortest paths from s to t
        (P nil)
        ;; number of shortest paths found to node u
        (count-u (make-hash-table)))
    ;; count-u = 0, for all u in V
    (loop for u below num-nodes do
         (setf (gethash u count-u) 0))
    ;; insert path Ps = {s} into B with cost 0
    (cl-heap:enqueue B (cons (list s) 0) 0)
    ;; while B is not empty and countt < K
    (loop while (and (not (= 0 (cl-heap:queue-size B)))
                     (< (gethash t_ count-u 0) k))
       do
       ;; let Pu be the shortest cost path in B with cost C
         ;; (format t "entered loop count-t: ~A~%" (gethash t_ count-u 0))
         (let* ((pair (cl-heap:dequeue B))
                (Pu (car pair))
                (C (cdr pair))
                (u (car Pu)))
           ;; countu = countu + 1
           (incf (gethash u count-u 0))
           ;; if u = t then P = P + Pu
           (when (eql u t_) (push (reverse Pu) P))
           ;; if countu ≤ K then
           ;; (format t ">> ~A~%" (< (gethash u count-u) k))
           (when (< (gethash u count-u) k)
             ;; for each vertex v adjacent to u
             (mapc-node-edges
              g u (lambda (v _)
                    (declare (ignore _))
                    ;; (format t ">> ~A -> ~A~%" u v)
                    ;; if v is not in Pu then
                    (unless (member v Pu) ;; TODO may be costly let Pv
                      ;; be a new path with cost C + w(u, v) formed by
                      ;; concatenating edge (u, v) to path Pu
                      (let ((Pv (cons v Pu))
                            (PvC (1+ C)))
                        ;; insert Pv into B
                        (cl-heap:enqueue B (cons Pv PvC) PvC))))))))
    (reverse P)))

(defun test-aux (spec k from to expected)
  (let* ((nodes (remove-duplicates
                 (alexandria:flatten spec)))
         (g (let ((g (make-graph 'hash-graph
                                 :num-nodes (length nodes))))
              (loop for (a b) in spec do
                   (add-edge g a b nil))
              g))
         (P (k-shortest g (length nodes) k from to)))
    
    (assert (equalp P expected))
    t))

(defun test.1 ()
  (test-aux '((0 1)
              (1 2)
              (2 5)
              (1 3)
              (2 4)
              (3 4)
              (4 5))
            2 0 5
            '((0 1 2 5) (0 1 2 4 5))))

(defun test.2 ()
  (test-aux '((0 1)
              (1 2)
              (2 5)
              (1 3)
              (2 4)
              (3 4)
              (4 5))
            3 0 5
            '((0 1 2 5) (0 1 3 4 5) (0 1 2 4 5))))

(defun test.3 ()
  (test-aux '((0 1)
              (1 2)
              (1 3)
              (2 4)
              (3 4))
            2 0 5
            nil))
