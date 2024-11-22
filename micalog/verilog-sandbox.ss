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
      (output out-and (and in-1 in-2)))))

(newline)

(display
  (verilog-string
    (module counter-4
      (input clock-0 1)
      (register clock-1 1)
      (register clock-2 1)
      (register clock-3 1)
      (on clock-0 (posedge (set clock-1 (not clock-1))))
      (on clock-1 (posedge (set clock-2 (not clock-2))))
      (on clock-2 (posedge (set clock-3 (not clock-3))))
      (output counter (append clock-3 clock-2 clock-1 clock-0)))))

(newline)

(display
  (verilog-string
    (module cascading-counter-4
      (input clock-0 1)
      (register clock-1 1)
      (register clock-2 1)
      (register clock-3 1)
      (on clock-0
        (posedge
          (set clock-1 (not clock-1))
          (on clock-1
            (posedge
              (set clock-2 (not clock-2))
              (on clock-2
                (posedge
                  (set clock-3 (not clock-3))))))))
      (output counter (append clock-3 clock-2 clock-1 clock-0)))))

(newline)

(display
  (verilog-string
    (module funny-module
      (input clock 1)
      (input reset? 1)
      (input mouse-pressed? 1)
      (input mouse-x 16)
      (register counter 16)
      (on clock
        (posedge
          (wire inc-counter (+ counter hex-0001))
          (wire dec-counter (- counter hex-0001))
          (wire updated-counter (if mouse-pressed? inc-counter dec-counter))
          (set counter (if reset? mouse-x updated-counter))))
      (output out counter))))
