(asdf:defsystem :src
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (
	       :src/main
	       :src/bfs
           :src/graph)
  ;;:in-order-to ((test-op (load-op )))
  ;;:perform ;; (test-op (o c)
           ;;          (lisp-unit:run-tests :all :src/test/field)
           ;;          (lisp-unit:run-tests :all :src/test/state)
           ;;          (lisp-unit:run-tests :all :src/test/parser))
  )

;;(register-system-packages :spatial-trees '(:rectangles))

