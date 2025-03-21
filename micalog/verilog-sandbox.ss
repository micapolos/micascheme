(import (micalog verilog) (micalog std))

(display-micalog-verilog
  (module empty)

  (module and-gate
    (input 8 in-1)
    (input 8 in-2)
    (output out-and (and in-1 in-2)))

  (module oscillating-registers
    (input clock)
    (input reset?)
    (input 8 initial)
    (register 8 reg-1)
    (register 8 reg-2)
    (on (posedge clock)
      (cond
        (reset?
          (set reg-1 initial)
          (set reg-2 (not initial)))
        (else
          (set reg-1 reg-2)
          (set reg-2 reg-1))))
    (output out reg-1))

  (module counter-4
    (input clock-0)
    (register clock-1)
    (register clock-2)
    (register clock-3)
    (on (posedge clock-0) (set-not clock-1))
    (on (posedge clock-1) (set-not clock-2))
    (on (posedge clock-2) (set-not clock-3))
    (output counter (append clock-3 clock-2 clock-1 clock-0)))

  (module cascading-counter-4
    (input clock-0)
    (register clock-1)
    (register clock-2)
    (register clock-3)
    (on (posedge clock-0)
      (set-not clock-1)
      (on (posedge clock-1)
        (set-not clock-2)
        (on (posedge clock-2)
          (set-not clock-3))))
    (output counter (append clock-3 clock-2 clock-1 clock-0)))

  (module funny-module
    (input clock)
    (input reset?)
    (input mouse-pressed?)
    (input 16 mouse-x)
    (register 16 counter)
    (on (posedge clock)
      (cond
        (reset? (set counter mouse-x))
        (mouse-pressed? (inc counter))
        (else (dec counter))))
    (output out counter)))
