(import
  (micascheme)
  (leo evaluator))

(check (obj=? (evaluate (evaluated `v `t)) (evaluated `v `t)))
(check (obj=? (evaluate (failure `foo)) (failure `foo)))

(check (obj=? (evaluate #f) (evaluated #f boolean!)))
(check (obj=? (evaluate #t) (evaluated #t boolean!)))

(check (obj=? (evaluate 128) (evaluated 128 number!)))
(check (obj=? (evaluate "foo") (evaluated "foo" string!)))

(check
  (obj=?
    (evaluate (tuple! #t 128 "foo"))
    (evaluated
      (tuple! #t 128 "foo")
      (tuple! boolean! number! string!))))
