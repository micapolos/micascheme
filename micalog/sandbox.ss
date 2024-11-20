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
        (clock
          (on
            (posedge
              (init
                (half-clock 1 0)
                (counter 16 0))
              (update
                (half-clock 1 (not 1 half-clock))
                (inc-counter 16 (add 16 counter 1))
                (dec-counter 16 (sub 16 counter 1))
                (updated-counter 16 (if 16 mouse-pressed? inc-counter dec-counter))
                (counter 16 (if 16 reset? mouse-x updated-counter)))))))
      (output
        (value 16 counter)))))
