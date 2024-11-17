(import
  (only (scheme) display)
  (verilog string))

(display
  (verilog-string
    (reg half-clock 0)
    (reg (range 7 0) counter-8 #b11111111)
    (wire (range 3 0) counter-4 (ref counter-8 (range 3 0)))
    (always (posedge clock)
      (set! half-clock (inv half-clock)))
    (always (posedge half-clock)
      (if write?
        (set! counter-8 (+ counter-8 1))))))
