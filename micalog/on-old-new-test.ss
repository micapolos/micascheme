(import
  (micascheme)
  (micalog on-old-new)
  (prefix (micalog keywords) %))

(define-check-datum-> on-old-new-syntax)

(check-on-old-new-syntax
  (statement
    (%on clock
      (%posedge a b)
      (%else c d)))
  (begin
    (%register 1 old-clock_0)
    (%set 1 old-clock_0 clock)
    (%on (old-clock_0 clock)
      (%posedge a b)
      (%else c d))))

(check-on-old-new-syntax
  (statement
    (%on clock
      (posedge (%on other-clock))
      (negedge (%on other-clock))))
  (begin
    (%register 1 old-clock_0)
    (%set 1 old-clock_0 clock)
    (%on (old-clock_0 clock)
      (posedge
        (%register 1 old-other-clock_1)
        (%set 1 old-other-clock_1 other-clock)
        (%on (old-other-clock_1 other-clock)))
      (negedge
        (%register 1 old-other-clock_2)
        (%set 1 old-other-clock_2 other-clock)
        (%on (old-other-clock_2 other-clock))))))

(check-on-old-new-syntax
  (statement
    (%cond
      (a b c)
      (d (%on clock))))
  (%cond
    (a b c)
    (d
      (%register 1 old-clock_0)
      (%set 1 old-clock_0 clock)
      (%on (old-clock_0 clock)))))

(check-on-old-new-syntax
  (module
    (%module foo
      (%on clock
        (%posedge a b)
        (%else c d))))
  (%module foo
    (%register 1 old-clock_0)
    (%set 1 old-clock_0 clock)
    (%on (old-clock_0 clock)
      (%posedge a b)
      (%else c d))))
