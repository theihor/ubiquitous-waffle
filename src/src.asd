(asdf:defsystem :src
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (
	       :src/main
               :src/game-state
               :src/game-protocol
	       )
  :in-order-to ((asdf:test-op (asdf:load-op :src/test/graph)
                              (asdf:load-op :src/test/game-state)))
  :perform (asdf:test-op
            (o c)
            (lisp-unit:run-tests :all :src/test/graph)
            (lisp-unit:run-tests :all :src/test/game-state))
  )

;;(register-system-packages :spatial-trees '(:rectangles))

