(import (micascheme) (react) (sequential))

(react
  (rect 0 0 mouse-x mouse-y)
  (rect
    mouse-x
    mouse-y
    (- canvas-width mouse-x)
    (- canvas-height mouse-y))
  (audio
    (lets
      ((sawtooth freq)
        (sequence 0 prev
          (fract (+ prev (/ freq sample-rate)))))

      ((fatty freq)
        (mix
          (sawtooth (* freq 1.0000))
          (sawtooth (* freq 1.0025))
          (sawtooth (* freq 1.0050))))

      ((glissando freq)
        (sequence freq prev
          (- freq (* 0.999 (- freq prev)))))

      (freq (+ 40 (* mouse-x 0.25)))
      (freq (if space? (* freq 2) freq))
      (freq (glissando freq))

      (tone (fatty freq))
      (beat (inverse (sawtooth 6)))
      (modulate tone beat))))
