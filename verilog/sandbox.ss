(import
  (only (scheme) display)
  (verilog string))

(display
  (verilog-string
    (reg half-clock 0)
    (reg (range 7 0) counter-8 #xff)
    (wire (range 3 0) counter-4)
    (always * (assign counter-4 (ref counter-8 (range 3 0))))
    (always (posedge clock) (set! half-clock (inv half-clock)))
    (always (posedge half-clock)
      (cond
        (reset? (set! counter-8 0))
        (increment? (set! counter-8 (+ counter-8 1)))
        (else (set! counter-8 (- counter-8 1)))))))
