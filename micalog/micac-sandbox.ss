(import
  (micalog micac)
  (only (micac run) micac-run-echo?)
  (only (c run) c-run-echo?))

(c-run-echo? #t)
(micac-run-echo? #t)

(micalog-emu
  (module colour-bars
    (input 1 clock)
    (input 9 video-x)
    (input 9 video-y)
    (input 9 mouse-x)
    (input 9 mouse-y)
    (input 1 mouse-pressed?)
    (register 64 red-counter)
    (register 64 green-counter)
    (register 64 blue-counter)
    (register 8 bar-red)
    (register 8 bar-green)
    (register 8 bar-blue)
    (on clock
      (posedge
        (set red-counter (+ red-counter 1))
        (set green-counter (+ green-counter 1))
        (set blue-counter (+ blue-counter 1))
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
    (wire screen?
      (and
        (and (>= video-x 48) (< video-x 304))
        (and (>= video-y 48) (< video-y 240))))
    (output red (if screen? hex-dd bar-red))
    (output green (if screen? hex-dd bar-green))
    (output blue (if screen? hex-dd bar-blue))))
