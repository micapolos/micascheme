(import (micascheme) (typed))

(check (works? v0))
(check (works? v1))
(check (works? v2))

(check (works? (tuple! (foo v0 v1))))
(check (works? (tuple-ref `tuple 1)))
(check (works? (application! v0 v1 v2)))
(check (works? (function! (foo `lhs `rhs) (tuple v0 v1))))
