(import (micascheme) (sjasm dupa))

(pretty-print
  (expand/optimize
    '(dupa
      (define len 10)
      (define foo)
      (display 10)
      (define end2)
      (newline)
      (define end)
      )
    (environment '(scheme) '(sjasm dupa))))
