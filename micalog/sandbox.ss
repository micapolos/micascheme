(import
  (micalog verilog-string)
  (only (micascheme) display newline))

(display
  (verilog-string
    (module and-gate
      (input 1 in-1)
      (input 1 in-2)
      (output 1 out-and (and 1 in-1 in-2))
      (output 1 out-nand (not 1 out-and)))))

(newline)

(display
  (verilog-string
    (module counter-4
      (input 1 clock)
      (on clock
        (posedge
          (register 1 clock-1)
          (set 1 clock-1 (not 1 clock-1))))
      (on clock-1
        (posedge
          (register 1 clock-2)
          (set 1 clock-2 (not 1 clock-2))))
      (on clock-2
        (posedge
          (register 1 clock-3)
          (set 1 clock-3 (not 1 clock-3))))
      (output 4 counter (append 4 clock-3 clock-2 clock-1 clock)))))

(newline)

(display
  (verilog-string
    (module alternative-counter-4
      (input 1 clock)
      (on clock
        (posedge
          (register 1 clock-1)
          (set 1 clock-1 (not 1 clock-1))
          (on clock-1
            (posedge
              (register 1 clock-2)
              (set 1 clock-2 (not 1 clock-2))
              (on clock-2
                (posedge
                  (register 1 clock-3)
                  (set 1 clock-3 (not 1 clock-3))))))))
      (output 4 counter (append 4 clock-3 clock-2 clock-1 clock)))))

(newline)

(display
  (verilog-string
    (module funny-module
      (input 1 clock)
      (input 1 reset?)
      (input 1 mouse-pressed?)
      (input 16 mouse-x)
      (output 16 counter)
      (on clock
        (posedge
          (register 1 half-clock)
          (register 16 counter)
          (set 1 half-clock (not 1 half-clock))
          (wire 16 inc-counter (add 16 counter 1))
          (wire 16 dec-counter (sub 16 counter 1))
          (wire 16 updated-counter (if 16 mouse-pressed? inc-counter dec-counter))
          (set 16 counter (if 16 reset? mouse-x updated-counter)))))))
