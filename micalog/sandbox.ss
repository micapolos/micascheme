(import
  (micalog verilog-string)
  (only (micascheme) display newline))

(display
  (verilog-string
    (module empty)))

(newline)

(display
  (verilog-string
    (module and-gate
      (input in-1 8)
      (input in-2 8)
      (output out-and (and in-1 in-2))
      (output out-nand (not out-and)))))

(newline)

(display
  (verilog-string
    (module alternative-counter-4
      (input clock 1)
      (on clock
        (posedge
          (register clock-1 1)
          (set clock-1 (not clock-1))
          (on clock-1
            (posedge
              (register clock-2 1)
              (set clock-2 (not clock-2))
              (on clock-2
                (posedge
                  (register clock-3 1)
                  (set clock-3 (not clock-3))))))))
      ; TODO: How to access register from sub-process?
      (output counter hex-0))))

(newline)

(display
  (verilog-string
    (module funny-module
      (input clock 1)
      (input reset? 1)
      (input mouse-pressed? 1)
      (input mouse-x 16)
      (on clock
        (posedge
          (register half-clock 1)
          (register counter 16)
          (set half-clock (not half-clock))
          (wire inc-counter (+ counter hex-0001))
          (wire dec-counter (- counter hex-0001))
          (wire updated-counter (if mouse-pressed? inc-counter dec-counter))
          (set counter (if reset? mouse-x updated-counter))))
      ; TODO: How to access register from sub-process?
      (output counter hex-0000))))
