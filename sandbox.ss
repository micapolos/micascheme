(import (micascheme) (react) (sequential))

(react
  (audio
    (lets
      (freq 110)
      (freq (if space? (* freq 1.5) freq))
      (freq (sequence freq prev (- freq (* 0.999 (- freq prev)))))
      (dx (/ freq 22050))
      (osc1 (sequence 0 x (fract (+ x dx))))
      (osc2 (sequence 0 x (fract (+ x (* dx 1.005)))))
      (osc3 (sequence 0 x (fract (+ x (* dx 1.010)))))
      (freq 8)
      (dx (/ freq 22050))
      (beat (- 1 (sequence 0 x (fract (+ x dx)))))
      (* beat (/ (+ osc1 osc2 osc3) 3)))))
