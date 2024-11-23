(import
  (micalog micac)
  (only (micac run) micac-run-echo?)
  (only (c run) c-run-echo?))

(c-run-echo? #t)
(micac-run-echo? #t)

(micalog-emu
  (module (previous-clock clock)
    (register 16 bar-counter)
    (register 8 color)
    (on (previous-clock clock)
      (posedge
        (set 16 bar-counter (- 16 bar-counter 1))
        (wire 1 bar-counter-zero? (= 16 bar-counter 0))
        (cond
          (bar-counter-zero?
            (set 16 bar-counter 9950)
            (set 8 color (not 8 color))
            (set 8 red color)))))))
