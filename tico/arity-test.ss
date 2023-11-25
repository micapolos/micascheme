(import (micascheme) (tico arity))

(check (equal? (arity+ (arity 2) (arity 3)) (arity 5)))
