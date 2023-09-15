(import (micascheme) (react))

(react
  (rect 0 0 mouse-x mouse-y)
  (rect mouse-x mouse-y 40 40)
  (message mouse-x)
  (message mouse-y)
  (message space?)
  (audio
    (*
      (/ mouse-y 480)
      (- 1 (osc (if space? 8 4)))
      (mix
        (osc (* mouse-x 1))
        (osc (* mouse-x 1.005))
        (osc (* mouse-x 1.001))))))
