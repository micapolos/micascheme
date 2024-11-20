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
        (register 1
          (init 0)
          (on (posedge clock) inv-half-clock)))

      (internal 1 previous-half-clock
        (register 1
          (init 0)
          (on (negedge clock) half-clock)))

      (internal 1 next-half-clock
        (not 16 previous-half-clock))

      (internal 16 counter
        (register 16
          (init)
          (on (posedge half-clock) next-counter)))

      (internal 16 previous-counter
        (register 16
          (init)
          (on (negedge half-clock) counter)))

      (internal 16 inc-counter
        (add 16 previous-counter 1))

      (internal 16 dec-counter
        (sub 16 previous-counter 1))

      (internal 16 updated-counter
        (if 16 mouse-pressed? inc-counter dec-counter))

      (internal 16 next-counter
        (if 16 reset? mouse-x updated-counter))

      (output 16 value counter))))
