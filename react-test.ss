(import (micascheme) (react))

(react
  (message (format "Frames: ~s" frames))
  (message (format "Time: ~,2f seconds" seconds))
  (message (format "Mouse: ~s ~s" mouse-x mouse-y))
  (message (format "Space: ~s" space-pressed?))
  (rect
    (- mouse-x 15)
    (- mouse-y 15)
    30 30)
  (rect
    (+ 320 -30 (* (sin seconds) 200))
    (+ 240 -30 (* (cos seconds) 200))
    60 60)
  (audio
    (*
      (min 1 (/ mouse-y 480))
      (/
        (+
          (osc (* 1.000 mouse-x))
          (osc (* 1.005 mouse-x))
          (osc (* 1.010 mouse-x)))
        3)
      (- 1
        (osc (if space-pressed? 8 4)))))
)
