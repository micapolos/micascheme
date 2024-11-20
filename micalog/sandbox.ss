(import
  (micalog verilog-string)
  (only (micascheme) display newline))

(display
  (verilog-string
    (module and-gate
      (input
        (in-1 1)
        (in-2 1))
      (internal)
      (output
        (out-and 1 (and 1 in-1 in-2))
        (out-nand 1 (not 1 out-and))))))

(newline)

(display
  (verilog-string
    (module funny-counter
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
        (out 16 counter)))))
