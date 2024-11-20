(import
  (micascheme)
  (micalog model)
  (prefix (micalog keywords) %))

(check (syntax=? (type-size #'6) #'6))

(check (syntax=? (expr-type #'(%expr foo _)) #'foo))
(check (syntax=? (expr-value #'(%expr _ foo)) #'foo))

(check (syntax=? (reg-type #'(%reg 6)) #'6))

(check (syntax=? (process-edge #'(%posedge _ _)) #'%posedge))
(check (syntax=? (process-edge #'(%negedge _ _)) #'%negedge))

(check (opposite-edges? #'%posedge #'%negedge))
(check (opposite-edges? #'%negedge #'%posedge))

(check (raises (opposite-edges? #'%negedge #'%negedge)))
(check (raises (opposite-edges? #'%posedge #'%posedge)))

(check (opposite-processes? #'(%posedge _ _) #'(%negedge _ _)))
(check (opposite-processes? #'(%negedge _ _) #'(%posedge _ _)))
(check (raises (opposite-processes? #'(%posedge _ _) #'(%posedge _ _))))
(check (raises (opposite-processes? #'(%negedge _ _) #'(%negedge _ _))))

(check-flattens (%module mod) (%module mod))

(check-flattens
  (%module mod
    (%on clock
      (%posedge
        (%input 1 in)
        (%output 1 out var)
        (%register 1 foo)
        (%wire 1 var gar)
        (%set 1 foo bar))))
  (%module mod
    (%input 1 in)
    (%output 1 out var)
    (%register 1 foo)
    (%wire 1 var gar)
    (%on clock
      (%posedge
        (%set 1 foo bar)))))

(check-flattens
  (%module mod
    (%on clock
      (%posedge
        (%register 1 reg-1)
        (%set 1 reg-1 val-1))
      (%negedge
        (%register 2 reg-2)
        (%set 2 reg-2 val-2))))
  (%module mod
    (%register 1 reg-1)
    (%register 2 reg-2)
    (%on clock
      (%posedge
        (%set 1 reg-1 val-1))
      (%negedge
        (%set 2 reg-2 val-2)))))

(check-flattens
  (%module mod
    (%on clock
      (%posedge
        (%register 1 reg-1)
        (%set 1 reg-1 val-1)
        (%on clock-2
          (%negedge
            (%register 2 reg-2)
            (%set 2 reg-2 val-2))))))
  (%module mod
    (%register 1 reg-1)
    (%register 2 reg-2)
    (%on clock-2
      (%negedge
        (%set 2 reg-2 val-2)))
    (%on clock
      (%posedge
        (%set 1 reg-1 val-1)))))



