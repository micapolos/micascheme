(import
  (micalog verilog-string)
  (only (micascheme) display))

(display
  (verilog-string
    (module sandbox
      (input
        (clock 1)
        (reset? 1)
        (mouse-pressed? 1)
        (mouse-x 16))
      (internal
        (half-clock 1
          (register 1
            (init 0)
            (on (posedge clock) inv-half-clock)))
        (previous-half-clock 1
          (register 1
            (init 0)
            (on (negedge clock) half-clock)))
        (next-half-clock 1
          (not 16 previous-half-clock))
        (counter 16
          (register 16
            (init)
            (on (posedge half-clock) next-counter)))
        (previous-counter 16
          (register 16
            (init)
            (on (negedge half-clock) counter)))
        (inc-counter 16
          (add 16 previous-counter 1))
        (dec-counter 16
          (sub 16 previous-counter 1))
        (updated-counter 16
          (if 16 mouse-pressed? inc-counter dec-counter))
        (next-counter 16
          (if 16 reset? mouse-x updated-counter)))
      (output
        (value 16 counter)))))
