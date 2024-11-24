(import
  (micalog micac)
  (only (micac run) micac-run-echo?)
  (only (c run) c-run-echo?))

(c-run-echo? #t)
(micac-run-echo? #t)

(micalog-emu
  (module (previous-clock clock)
    (register 16 red-counter)
    (register 16 green-counter)
    (register 16 blue-counter)
    (on (previous-clock clock)
      (posedge
        (set 16 red-counter (- 16 red-counter 1))
        (set 16 green-counter (- 16 green-counter 1))
        (set 16 blue-counter (- 16 blue-counter 1))
        (cond
          ((= 16 red-counter 0)
            (set 16 red-counter 19940)
            (set 8 red (not 8 red))))
        (cond
          ((= 16 green-counter 0)
            (set 16 green-counter 19920)
            (set 8 green (not 8 green))))
        (cond
          ((= 16 blue-counter 0)
            (set 16 blue-counter 19900)
            (set 8 blue (not 8 blue))))))))
