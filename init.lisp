(load #P"~/quicklisp/setup.lisp")

(ql:quickload 'asdf)

(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)

;; (ql:quickload 'fset)
(ql:quickload 'lisp-unit)
;; (ql:quickload 'cl-quickcheck)
(ql:quickload 'alexandria)
;; (ql:quickload 'cl-graph)
;; (ql:quickload 'cl-heap)
;; (ql:quickload 'spatial-trees)
;; (ql:quickload 'spatial-trees.nns)
(ql:quickload 'apply-argv)
;; (ql:quickload 'ironclad)
;; (ql:quickload 'babel)
;; (ql:quickload 'cl-svg)
(ql:quickload 'yason)
(ql:quickload 'drakma)
;; (ql:quickload 'cl-geometry)
;; (ql:quickload 'smug)
(ql:quickload 'anaphora)
;; (ql:quickload 'cl-fad)
;; (ql:quickload 'cl-cairo2)
;; (ql:quickload 'cl-containers)

;; (ql:quickload 'log4cl)
;; (ql:quickload 'closer-mop)
;; (ql:quickload 'bordeaux-threads)
;; (ql:quickload 'trivial-garbage)
;; (ql:quickload 'osicat)
;; (ql:quickload 'stmx)
;; (ql:quickload 'hyperluminal-mem)
;; (ql:quickload 'trivial-timeout)
(ql:quickload 'cl-ppcre)
(ql:quickload 'cl-mop)

(ql:quickload 'trivial-timeout)
;; (ql:quickload :cl-rl)

(ql:quickload 'cl-heap)

(in-package :cl-user)

(require 'sb-sprof)

(asdf:initialize-source-registry '(:source-registry
                                   :inherit-configuration
                                   (:directory :here)
                                   (:directory (:here "src/"))))

(asdf:compile-system :src)
(asdf:load-system :src)

(print "Welcome.")






