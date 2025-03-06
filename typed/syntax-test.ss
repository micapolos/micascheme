(import (micascheme) (typed syntax) (typed type))

(define-type (ll component))

(check (equal? (syntax->datum (type->syntax (ll any-string))) `(ll any-string)))
