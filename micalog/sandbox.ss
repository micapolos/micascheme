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
    (module clock-counter
      (input
        (clock 1))
      (internal
        (on clock
          (posedge
            (init (register 1 clock-1))
            (update (set 1 clock-1 (not 1 clock-1)))))
        (on clock-1
          (posedge
            (init (register 1 clock-2))
            (update (set 1 clock-2 (not 1 clock-2)))))
        (on clock-2
          (posedge
            (init (register 1 clock-3))
            (update (set 1 clock-3 (not 1 clock-3))))))
      (output
        (counter 4 (append 4 clock-3 clock-2 clock-1 clock))))))

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
        (on clock
          (posedge
            (init
              (register 1 half-clock)
              (register 16 counter))
            (update
              (set 1 half-clock (not 1 half-clock))
              (wire 16 inc-counter (add 16 counter 1))
              (wire 16 dec-counter (sub 16 counter 1))
              (wire 16 updated-counter (if 16 mouse-pressed? inc-counter dec-counter))
              (set 16 counter (if 16 reset? mouse-x updated-counter))))))
      (output
        (out 16 counter)))))
