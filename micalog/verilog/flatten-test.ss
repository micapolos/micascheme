(import
  (micascheme)
  (micalog verilog flatten)
  (prefix (micalog keywords) %))

(check-flattens (%module mod) (%module mod))

(check-flattens
  (%module mod
    (%on (%posedge clock)
      (%input 1 in)
      (%output 1 out var)
      (%register 1 foo)
      (%wire 1 var gar)
      (%set 1 foo bar)))
  (%module mod
    (%input 1 in)
    (%output 1 out)
    (%assign 1 out var)
    (%register 1 foo)
    (%wire 1 var)
    (%assign 1 var gar)
    (%on (%posedge clock)
      (%set 1 foo bar))))

(check-flattens
  (%module mod
    (%on (%posedge clock)
      (%register 1 reg-1)
      (%set 1 reg-1 val-1)
      (%on (%negedge clock-2)
        (%register 2 reg-2)
        (%set 2 reg-2 val-2))))
  (%module mod
    (%register 1 reg-1)
    (%register 2 reg-2)
    (%on (%negedge clock-2)
      (%set 2 reg-2 val-2))
    (%on (%posedge clock)
      (%set 1 reg-1 val-1))))

(check-flattens
  (%module mod
    (%cond
      (clock
        (%register 1 reg-1)
        (%set 1 reg-1 val-1))))
  (%module mod
    (%register 1 reg-1)
    (%cond (clock (%set 1 reg-1 val-1)))))

(check-flattens
  (%module mod
    (%cond
      (clock
        (%register 1 reg-1)
        (%set 1 reg-1 val-1))
      (%else
        (%register 2 reg-2)
        (%set 1 reg-2 val-2))))
  (%module mod
    (%register 1 reg-1)
    (%register 2 reg-2)
    (%cond
      (clock (%set 1 reg-1 val-1))
      (%else (%set 1 reg-2 val-2)))))

