(import (micalog emu) (micalog std))

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
    (register 16 frame-counter)
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
            (set-not bar-red)))
        (cond
          ((> green-counter 19920)
            (set green-counter 0)
            (set-not bar-green)))
        (cond
          ((> blue-counter 19900)
            (set blue-counter 0)
            (set-not bar-blue)))
        (cond
          ((and (= video-x 0) (= video-y 0))
            (inc frame-counter)))))
    (wire screen?
      (and
        (and (>= video-x 48) (< video-x 304))
        (and (>= video-y 48) (< video-y 240))))

    (wire plasma?
      (xor
        (> video-x mouse-x)
        (< video-y mouse-y)))

    (wire bar? (xor screen? mouse-pressed?))

    (wire counter-8 (slice frame-counter 8))
    (wire video-x-8 (slice video-x 8))
    (wire video-y-8 (slice video-y 8))
    (wire video-x-18 (append bin-000000000 video-x))
    (wire video-y-18 (append bin-000000000 video-y))

    (wire plasma-red (- counter-8 video-x-8))
    (wire plasma-green (- counter-8 video-y-8))
    (wire plasma-blue (+ counter-8 (slice (* video-x-18 video-y-18) 6 8)))

    (wire screen-red (if plasma? plasma-red hex-dd))
    (wire screen-green (if plasma? plasma-green hex-dd))
    (wire screen-blue (if plasma? plasma-blue hex-dd))

    (output video-red (if bar? screen-red bar-red))
    (output video-green (if bar? screen-green bar-green))
    (output video-blue (if bar? screen-blue bar-blue))))
