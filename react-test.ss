(import (micascheme) (react) (sequential))

(react
  (message (format "Frames: ~s" frames))
  (message (format "Time: ~,2f seconds" seconds))
  (message (format "Mouse: ~s ~s" mouse-x mouse-y))
  (message (format "Space: ~a" (if space? "yes" "no")))

  (rect
    (- mouse-x 15)
    (- mouse-y 15)
    30 30)
  (rect
    (+ 320 -30 (* (sin seconds) 200))
    (+ 240 -30 (* (cos seconds) 200))
    60 60)
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
      (* beat (/ (+ osc1 osc2 osc3) 3))))
)
