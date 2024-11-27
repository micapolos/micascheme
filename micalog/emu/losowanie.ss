(import
  (micalog emu)
  (micalog std))

(micalog-emu
  (module losowanie
    (input clock)
    (input reset?)
    (input mouse-pressed?)
    (register 2 counter)
    (register 2 result)
    (register done?)
    (on (posedge clock)
      (cond
        (reset?
          (set counter 0)
          (set done? 0))
        (else
          (inc counter)
          (when (and mouse-pressed? (not done?))
            (set result counter)
            (set done? 1)
            (log wynik (+ result 1))))))))
