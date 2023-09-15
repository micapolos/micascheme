(import (micascheme) (react))

(react
  (audio
    (mix
      (*
        (- 1 (osc 4))
        (osc
          (vector-ref
            (vector 220 330 440 440)
            (steps (osc 1) 4))))
      (*
        (- 1 (osc 2))
        (osc
          (vector-ref
            (vector 660 880 783 880 660 660 783 440)
            (steps (osc 0.25) 8))))
      (*
        (- 1 (osc 8))
        (- 1 (* 0.5 (osc 4)))
        (osc 110)))))
