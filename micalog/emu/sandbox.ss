(import (micalog emu) (micalog std))

(micalog-emu
  (module colour-bars
    (input reset?)
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

    (register 8 red)
    (register 8 green)
    (register 8 blue)

    (output video-red red)
    (output video-green green)
    (output video-blue blue)

    (on (posedge clock)
      (set-not half-clock))

    (on (posedge half-clock)
      (cond
        ((or reset? mouse-pressed?)
          (set frame-counter 0)
          (set bar-red 0)
          (set bar-green 0)
          (set bar-blue 0)
          (set red-counter 0)
          (set green-counter 0)
          (set blue-counter 0)
          (set red 0)
          (set green 0)
          (set blue 0))
        (else
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
            (inc frame-counter))

          (wire screen?
            (and
              (and (>= video-x 48) (< video-x 304))
              (and (>= video-y 48) (< video-y 240))))

          (wire plasma?
            (xor
              (> video-x mouse-x)
              (< video-y mouse-y)))

          (wire plasma-red (take (wrap- frame-counter video-x) 8))
          (wire plasma-green (take (wrap- frame-counter video-y) 8))
          (wire plasma-blue (take (+ frame-counter (drop (* video-x video-y) 6)) 8))

          (wire screen-red (if plasma? plasma-red hex-dd))
          (wire screen-green (if plasma? plasma-green hex-dd))
          (wire screen-blue (if plasma? plasma-blue hex-dd))

          (set red (if screen? screen-red bar-red))
          (set green (if screen? screen-green bar-green))
          (set blue (if screen? screen-blue bar-blue)))))))
