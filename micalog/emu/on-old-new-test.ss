(import
  (micascheme)
  (micalog emu on-old-new)
  (prefix (micalog keywords) %))

(define-check-datum-> on-old-new-syntax)

(check-on-old-new-syntax
  (statement
    (%on clock
      (%posedge a b)
      (%else c d)))
  (begin
    (%register 1 old-clock_0)
    (%on (old-clock_0 clock)
      (%posedge a b)
      (%else c d))
    (%set 1 old-clock_0 clock)))

(check-on-old-new-syntax
  (statement
    (%on clock
      (posedge (%on other-clock))
      (negedge (%on other-clock))))
  (begin
    (%register 1 old-clock_0)
    (%on (old-clock_0 clock)
      (posedge
        (%register 1 old-other-clock_1)
        (%on (old-other-clock_1 other-clock))
        (%set 1 old-other-clock_1 other-clock))
      (negedge
        (%register 1 old-other-clock_2)
        (%on (old-other-clock_2 other-clock))
        (%set 1 old-other-clock_2 other-clock)))
    (%set 1 old-clock_0 clock)))

(check-on-old-new-syntax
  (statement
    (%cond
      (a b c)
      (d (%on clock))))
  (%cond
    (a b c)
    (d
      (%register 1 old-clock_0)
      (%on (old-clock_0 clock))
      (%set 1 old-clock_0 clock))))

(check-on-old-new-syntax
  (module
    (%module foo
      (%on clock
        (%posedge a b)
        (%else c d))))
  (%module foo
    (%register 1 old-clock_0)
    (%on (old-clock_0 clock)
      (%posedge a b)
      (%else c d))
    (%set 1 old-clock_0 clock)))
