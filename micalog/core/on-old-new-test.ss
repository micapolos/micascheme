(import
  (micascheme)
  (micalog core on-old-new)
  (prefix (micalog keywords) %))

(define-check-datum-> on-old-new-syntax)

(check-on-old-new-syntax
  (statement (%on (%posedge clock) a b))
  (begin
    (%register 1 old-clock_0)
    (%on (%posedge old-clock_0 clock) a b)
    (%set 1 old-clock_0 clock)))

(check-on-old-new-syntax
  (statement
    (%cond
      (a b c)
      (d (%on (%posedge clock)))))
  (%cond
    (a b c)
    (d
      (%register 1 old-clock_0)
      (%on (%posedge old-clock_0 clock))
      (%set 1 old-clock_0 clock))))

(check-on-old-new-syntax
  (module
    (%module foo
      (%on (%posedge clock) a b)))
  (%module foo
    (%register 1 old-clock_0)
    (%on (%posedge old-clock_0 clock) a b)
    (%set 1 old-clock_0 clock)))
