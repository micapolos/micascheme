(import (micascheme) (react) (sequential))

(react
  (audio
    (lets
      ((sawtooth freq)
        (sequence 0 prev
          (fract (+ prev (/ freq sample-rate)))))

      ((fatty freq)
        (lets
          (a (sawtooth freq))
          (b (sawtooth (* freq 1.0025)))
          (c (sawtooth (* freq 1.0050)))
          (/ (+ a b c) 3)))

      ((glissando freq)
        (sequence freq prev
          (- freq (* 0.999 (- freq prev)))))

      (freq (+ 40 (* mouse-x 0.25)))
      (freq (if space? (* freq 2) freq))
      (freq (glissando freq))

      (tone (fatty freq))
      (beat (- 1 (sawtooth 6)))
      (* tone beat))))
