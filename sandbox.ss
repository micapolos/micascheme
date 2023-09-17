(import (micascheme) (react))

(react
  (audio
    (lets
      ($freq mouse-x)
      ($osc (- 1 (osc 4)))
      ($osc2 (fract (* 2 $osc)))
      (mix
        (* $osc (osc $freq))
        (* $osc2 (osc (* 1.5 $freq)))))))
