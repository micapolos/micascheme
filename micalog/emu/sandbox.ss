(import (micalog emu))

(micalog-emu
  (module colour-bars
    (input clock)
    (input 9 video-x)
    (input 9 video-y)
    (input 9 mouse-x)
    (input 9 mouse-y)
    (input mouse-pressed?)
    (register 64 red-counter)
    (register 64 green-counter)
    (register 64 blue-counter)
    (register 8 bar-red)
    (register 8 bar-green)
    (register 8 bar-blue)
    (on clock
      (posedge
        (inc red-counter)
        (inc green-counter)
        (inc blue-counter)
        (cond
          ((> red-counter 19940)
            (set red-counter 0)
            (set bar-red (not bar-red))))
        (cond
          ((> green-counter 19920)
            (set green-counter 0)
            (set bar-green (not bar-green))))
        (cond
          ((> blue-counter 19900)
            (set blue-counter 0)
            (set bar-blue (not bar-blue))))))
    (wire bar?
      (xor
        (not mouse-pressed?)
        (and
          (and (>= video-x 48) (< video-x 304))
          (and (>= video-y 48) (< video-y 240)))))
    (wire black? (xor (> video-x mouse-x) (> video-y mouse-y)))
    (wire background (if black? hex-00 hex-dd))
    (output red (if bar? bar-red background))
    (output green (if bar? bar-green background))
    (output blue (if bar? bar-blue background))))
