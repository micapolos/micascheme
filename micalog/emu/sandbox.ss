(import (micalog emu) (micalog std))

(micalog-emu
  (module colour-bars
    (input clock)
    (input 9 video-x)
    (input 9 video-y)
    (input 9 mouse-x)
    (input 9 mouse-y)
    (input mouse-pressed?)
    (register 32 red-counter)
    (register 32 green-counter)
    (register 32 blue-counter)
    (register 32 frame-counter)
    (register 8 bar-red)
    (register 8 bar-green)
    (register 8 bar-blue)
    (register 1 half-clock)

    (on (posedge clock)
      (set-not half-clock))

    (on (posedge half-clock)
      (inc red-counter)
      (inc green-counter)
      (inc blue-counter)
      (when (> red-counter 9980)
        (set red-counter 0)
        (set-not bar-red))
      (when (> green-counter 9960)
        (set green-counter 0)
        (set-not bar-green))
      (when (> blue-counter 9950)
        (set blue-counter 0)
        (set-not bar-blue))
      (when (and (= video-x 0) (= video-y 0))
        (inc frame-counter)))

    (wire screen?
      (and
        (and (>= video-x 48) (< video-x 304))
        (and (>= video-y 48) (< video-y 240))))

    (wire plasma?
      (xor
        (> video-x mouse-x)
        (< video-y mouse-y)))

    (wire bar? (xor screen? mouse-pressed?))

    (wire plasma-red (take (- frame-counter video-x) 8))
    (wire plasma-green (take (- frame-counter video-y) 8))
    (wire plasma-blue (take (+ frame-counter (drop (* video-x video-y) 6)) 8))

    (wire screen-red (if plasma? plasma-red hex-dd))
    (wire screen-green (if plasma? plasma-green hex-dd))
    (wire screen-blue (if plasma? plasma-blue hex-dd))

    (output video-red (if bar? screen-red bar-red))
    (output video-green (if bar? screen-green bar-green))
    (output video-blue (if bar? screen-blue bar-blue))))
