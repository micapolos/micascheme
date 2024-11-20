(import
  (micalog verilog-string)
  (only (micascheme) display)
  (micalog keywords))

(display
  (micalog->verilog-string
    (module sandbox
      (input clock 1)
      (input reset? 1)
      (input mouse-pressed? 1)
      (input mouse-x 16)

      (internal half-clock 1
        (register 1
          (init 0)
          (on (posedge clock) inv-half-clock)))

      (internal previous-half-clock 1
        (register 1
          (init 0)
          (on (negedge clock) half-clock)))

      (internal next-half-clock 1
        (not 16 previous-half-clock))

      (internal counter 16
        (register 16
          (init)
          (on (posedge half-clock) next-counter)))

      (internal previous-counter 16
        (register 16
          (init)
          (on (negedge half-clock) counter)))

      (internal inc-counter 16
        (add 16 previous-counter 1))

      (internal dec-counter 16
        (sub 16 previous-counter 1))

      (internal updated-counter 16
        (if 16 mouse-pressed? inc-counter dec-counter))

      (internal next-counter 16
        (if 16 reset? mouse-x updated-counter))

      (output value 16 counter))))
