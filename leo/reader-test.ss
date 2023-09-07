(import (micascheme) (leo reader))

(writeln
  (reader-value
    (reader-read-list
      (calculator-reader #f (lambda (_) (throw dupa)))
      `(
        pi
        (add pi)
        negate))))
