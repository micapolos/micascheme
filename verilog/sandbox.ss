(import
  (only (scheme) display)
  (verilog string))

(display
  (verilog-string
    (reg half-clock 0)

    (reg (7 : 0) counter-8 0)

    (wire (3 : 0) counter-4 (ref counter-8 (3 : 0)))

    (always (posedge clock)
      (set! half-clock (inv half-clock)))

    (always (posedge half-clock)
      (if write?
        (set! counter-8 (+ counter-8 1))))))
