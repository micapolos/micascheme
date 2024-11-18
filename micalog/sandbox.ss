(micalog
  (regs
    (clock (expr 1 ?))
    (reset? (expr 1 ?))
    (mouse-x (expr 16 ?))
    (mouse-y (expr 16 ?))
    (mouse-pressed? (expr 1 ?))
  (process
    (on (expr 1 clock)
      (posedge
        (val half-clock (expr (reg 1) (reg)))
        (val counter (expr (reg 8) (reg)))
        (val counter+16 (expr 8
          (+
            (expr 8 (reg-ref (expr (reg 8) counter)))
            (expr 8 16))))
        (on-edge (expr 1 half-clock)
          (positive
            (set!
              (expr (reg 8) counter)
              (expr 8
                (reg-ref (expr (reg 8) next-counter)))))
          (negative
            (cond
              ((expr 1 reset?)
                (set!
                  (expr (reg 8 next-counter))
                  (expr 8 (slice (expr 16 mouse-x) 0 8))))
              ((expr 1 mouse-pressed?)
                (set!
                  (expr (reg 8) next-counter)
                  (expr 8
                    (+
                      (expr 8 counter)
                      (expr 8 1)))))
              (else
                (set!
                  (expr (reg 8) next-counter)
                  (expr 8
                    (-
                      (expr 8 counter)
                      (expr 8 1)))))))))
      (negedge
        (reg (1 next-half-clock) (1 0))
        (reg (8 next-counter) (1 0))
        (set! (1 next-half-clock) (1 (not (1 half-clock))))))))
