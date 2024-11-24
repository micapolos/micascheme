(import
  (micalog micac)
  (only (micac run) micac-run-echo?)
  (only (c run) c-run-echo?))

(c-run-echo? #t)
(micac-run-echo? #t)

(micalog-emu
  (module colour-bars
    (input 1 clock)
    (register 16 red-counter)
    (register 16 green-counter)
    (register 16 blue-counter)
    (register 8 red)
    (register 8 green)
    (register 8 blue)
    (on clock
      (posedge
        (set red-counter (- red-counter 1))
        (set green-counter (- green-counter 1))
        (set blue-counter (- blue-counter 1))
        (cond
          ((= red-counter 0)
            (set red-counter 19940)
            (set red (not red))))
        (cond
          ((= green-counter 0)
            (set green-counter 19920)
            (set green (not green))))
        (cond
          ((= blue-counter 0)
            (set blue-counter 19900)
            (set blue (not blue))))))))
