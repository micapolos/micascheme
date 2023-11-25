(import (micascheme) (tico arity))

(check (equal? (arity+ (arity 2) (arity 3)) (arity (+ 2 3))))

(check (equal? (arity-single? (arity 0)) #f))
(check (equal? (arity-single? (arity 1)) #t))
(check (equal? (arity-single? (arity 2)) #f))
