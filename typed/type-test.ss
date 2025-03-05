(import (micascheme) (typed type))

(check (type=? 'int 'int))
(check (not (type=? 'int 'float)))

(check (type=? #'string? #'string?))
(check (not (type=? #'string? #'fixnum?)))

(check (type=? (arrow (list 'k1 'k2) 'v) (arrow (list 'k1 'k2) 'v)))
(check (not (type=? (arrow (list 'k1) 'v) (arrow (list 'k1 'k2) 'v))))
(check (not (type=? (arrow (list 'k1 'k1) 'v) (arrow (list 'k1 'k2) 'v))))
(check (not (type=? (arrow (list 'k1 'k2) 'v) (arrow (list 'k1 'k2) 'v2))))
