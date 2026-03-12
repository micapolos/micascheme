(import (leo scheme))

(library (leo (foo bar))
  (export foo bar)
  (import micascheme)
  (define foo 10)
  (define bar 20))

(import (leo (foo bar)))

(pretty-print foo)
(pretty-print bar)
