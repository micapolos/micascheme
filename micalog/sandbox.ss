(import
  (micalog verilog-string)
  (only (micascheme) display)
  (micalog keywords))

(display
  (micalog->verilog-string
    (module
      (input 1 clock)
      (input 1 reset?)
      (input 1 mouse-pressed?)
      (input 16 mouse-x)
      (internal 1 half-clock
        (register 1 0 clock 1 inv-half-clock))
      (internal 1 previous-half-clock
        (register 1 0 clock 0 half-clock))
      (internal 1 next-half-clock
        (not 16 previous-half-clock))
      (internal 16 previous-counter
        (register 16 0 half-clock 0 counter))
      (internal 16 inc-counter
        (inc 16 previous-counter))
      (internal 16 dec-counter
        (dec 16 previous-counter))
      (internal 16 next-counter
        (cond 16
          (reset? mouse-x)
          (mouse-pressed? inc-counter)
          (else dec-counter)))
      (output 16 counter
        (register 16 0 half-clock 1 next-counter)))))
