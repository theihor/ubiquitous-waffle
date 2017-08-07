(asdf:defsystem :src
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:src/main
               :src/game-state
               :src/game-protocol
               :src/graph
               :src/bfs
               :src/game-player
               :src/decode
               :src/mcts
               :src/simulator

               ;; :cl-rl
               ;; :src/rl/player
               ;; :src/rl/puntering
               ;; :src/rl/run
               )
  :in-order-to ((asdf:test-op (asdf:load-op :src/test/graph)
                              (asdf:load-op :src/test/game-state)
                              (asdf:load-op :src/test/game-player)))
  :perform (asdf:test-op
            (o c)
            (lisp-unit:run-tests :all :src/test/graph)
            (lisp-unit:run-tests :all :src/test/game-state)
            (lisp-unit:run-tests :all :src/test/game-player))
  )

;;(register-system-packages :spatial-trees '(:rectangles))

