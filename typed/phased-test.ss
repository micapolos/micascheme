(import (micascheme) (syntax lookup) (typed phased) (typed typed) (any))

(lets
  ($lookup
    (lookup-with
      (phase-0-foo (phased 0 (typed any-string #'"foo")))
      (phase-1-any-string (phased 1 (typed any-type any-string)))
      (phase-2-any-string (phased 2 (typed any-type any-string)))))
  ($phased-0 ((type-lookup $lookup) #'phase-0-foo))
  (run (check (false? $phased-0)))
  ($phased-1 ((type-lookup $lookup) #'phase-1-any-string))
  (run
    (check (equal? (phased-phase $phased-1) 0))
    (check (equal? (typed-type (phased-value $phased-1)) any-type))
    (check (syntax=? (typed-value (phased-value $phased-1)) #'any-string)))
  ($phased-2 ((type-lookup $lookup) #'phase-2-any-string))
  (run (check (equal? $phased-2 (phased 1 (typed any-type any-string))))))
