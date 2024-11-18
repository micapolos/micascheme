(micalog
  (val clock (: 1 ?))
  (val reset? (: 1 ?))
  (val mouse-x (: 16 ?))
  (val mouse-y (: 16 ?))
  (val mouse-pressed? (: 1 ?))
  (on (: 1 clock)
    (posedge
      (val half-clock (: (reg 1) (reg)))
      (val counter (: (reg 8) (reg)))
      (val counter+16 (: 8
        (+
          (: 8 (reg-ref (: (reg 8) counter)))
          (: 8 16))))
      (on (: 1 half-clock)
        (posedge
          (set!
            (: (reg 8) counter)
            (: 8
              (reg-ref (: (reg 8) next-counter)))))
        (negedge
          (cond
            ((: 1 reset?)
              (set!
                (: (reg 8 next-counter))
                (: 8 (slice (: 16 mouse-x) 0 8))))
            ((: 1 mouse-pressed?)
              (set!
                (: (reg 8) next-counter)
                (: 8
                  (+
                    (: 8 counter)
                    (: 8 1)))))
            (else
              (set!
                (: (reg 8) next-counter)
                (: 8
                  (-
                    (: 8 counter)
                    (: 8 1)))))))))
    (negedge
      (reg (1 next-half-clock) (1 0))
      (reg (8 next-counter) (1 0))
      (set! (1 next-half-clock) (1 (not (1 half-clock)))))))
