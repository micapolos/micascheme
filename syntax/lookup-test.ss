(import (micascheme) (syntax lookup))

(check (false? ((empty-lookup) #'zero)))

(lets
  ($lookup
    (fluent (empty-lookup)
      (lookup+ #'zero #'10)
      (lookup+ #'one #'1)
      (lookup+ #'zero #'0)))
  (run
    (check-datum=? ($lookup #'zero) #'0)
    (check-datum=? ($lookup #'one) #'1)
    (check (false? ($lookup #'two)))

    (check-datum=? (lookup-ref $lookup #'zero) #'0)
    (check-datum=? (lookup-ref $lookup #'one) #'1)
    (check (raises (lookup-ref $lookup #'two)))

    (check (raises (lookup+undefined $lookup #'one 11)))))

(lets
  ($lookup (lookup-with (zero 0) (one 1)))
  (run
    (check (equal? ($lookup #'zero) 0))
    (check (equal? ($lookup #'one) 1))
    (check (false? ($lookup #'two)))))

(lets
  ($lookup
    (lookup-append
      (lookup-with (zero 0) (one 11))
      (lookup-with (two 2) (one 1))))
  (run
    (check (equal? ($lookup #'zero) 0))))
