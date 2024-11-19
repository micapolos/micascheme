(import
  (only (scheme) display)
  (verilog string))

(display
  (verilog-string
    (module
      (funny-counter
        (input clock)
        (input reset?)
        (input mouse-pressed?)
        (input (15 to 0) mouse-x)
        (output (8 to 0) value))
      (reg half-clock 0)
      (reg (7 to 0) counter #xff)
      (always (posedge clock)
        (set! half-clock (inv half-clock)))
      (always (posedge half-clock)
        (cond
          (reset? (set! counter (ref mouse-x (7 to 0))))
          (mouse-pressed? (set! counter (+ counter 1)))
          (else (set! counter (- counter 1)))))
      (always *
        (assign value counter)))))
