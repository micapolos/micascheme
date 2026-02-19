(import (leo2 base) (leo2 term) (leo2 type-of))

(check (equal? (type-of (type 0)) (type 1)))
(check (equal? (type-of (type 1)) (type 2)))

(check
  (equal?
    (type-of
      (typed
        (typed (type 0) (native 'a-boolean))
        (native #t)))
    (typed (type 0) (native 'a-boolean))))
