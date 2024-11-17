(import
  (only (scheme) display)
  (verilog string))

(display
  (verilog-string
    (reg half-clock 0)
    (reg (7 to 0) counter-8 #xff)
    (wire (3 to 0) counter-4)
    (always * (assign counter-4 (ref counter-8 (3 to 0))))
    (always (posedge clock) (set! half-clock (inv half-clock)))
    (always (posedge half-clock)
      (cond
        (reset? (set! counter-8 0))
        (increment? (set! counter-8 (+ counter-8 1)))
        (else (set! counter-8 (- counter-8 1)))))))
