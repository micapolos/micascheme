(import (micascheme) (react))

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
    (*
      (min 1 (/ mouse-y 480))
      (mix
        (* (max 0 (- 1 (* 8 (osc 8)))) noise)
        (osc (* 1.000 mouse-x))
        (osc (* 1.005 mouse-x))
        (osc (* 1.010 mouse-x))
      )
      (- 1 (osc (if space? 8 4)))))
)
