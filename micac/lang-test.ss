(import
  (micac lang))

(micac
  (externs printf)

  (macro (plusik a b) (+ a b))

  (run
    (var int x 10)
    (add x (plusik 20 30))
    (printf "Hello\\n")))
