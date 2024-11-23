(import (micalog verilog-syntax))

(display-verilog
  (module empty)

  (module and-gate
    (input in-1 8)
    (input in-2 8)
    (output out-and (and in-1 in-2)))

  (module oscillating-registers
    (input clock 1)
    (input reset? 1)
    (input initial 2)
    (register reg-1 2)
    (register reg-2 2)
    (on clock
      (posedge
        (cond
          (reset?
            (set reg-1 initial)
            (set reg-2 (not initial)))
          (else
            (set reg-1 reg-2)
            (set reg-2 reg-1)))))
    (output out reg-1))

  (module counter-4
    (input clock-0 1)
    (register clock-1 1)
    (register clock-2 1)
    (register clock-3 1)
    (on clock-0 (posedge (set clock-1 (not clock-1))))
    (on clock-1 (posedge (set clock-2 (not clock-2))))
    (on clock-2 (posedge (set clock-3 (not clock-3))))
    (output counter (append clock-3 clock-2 clock-1 clock-0)))

  (module cascading-counter-4
    (input clock-0 1)
    (register clock-1 1)
    (register clock-2 1)
    (register clock-3 1)
    (on clock-0
      (posedge (set clock-1 (not clock-1))
        (on clock-1
          (posedge (set clock-2 (not clock-2))
            (on clock-2
              (posedge (set clock-3 (not clock-3))))))))
    (output counter (append clock-3 clock-2 clock-1 clock-0)))

  (module funny-module
    (input clock 1)
    (input reset? 1)
    (input mouse-pressed? 1)
    (input mouse-x 16)
    (register counter 16)
    (on clock
      (posedge
        (cond
          (reset? (set counter mouse-x))
          (mouse-pressed? (set counter (+ counter hex-0001)))
          (else (set counter (- counter hex-0001))))))
    (output out counter)))
