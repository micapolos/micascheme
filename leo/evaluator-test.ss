(import
  (except (micascheme) pair)
  (leo evaluator))

(check (equal? (evaluate (evaluated `v `t)) (evaluated `v `t)))
(check (equal? (evaluate (failure `foo)) (failure `foo)))

(check (equal? (evaluate #f) (evaluated #f boolean!)))
(check (equal? (evaluate #t) (evaluated #t boolean!)))

(check (equal? (evaluate 128) (evaluated 128 number!)))
(check (equal? (evaluate "foo") (evaluated "foo" string!)))

(check
  (equal?
    (evaluate (tuple! #t 128 "foo"))
    (evaluated
      (tuple! #t 128 "foo")
      (tuple! boolean! number! string!))))
