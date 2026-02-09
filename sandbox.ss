(import (micascheme))

(displayln
  (letrec
    ((zeros-string
      (lambda (n)
        (if (zero? n) "" (string-append (zeros-string (- n 1)) "0")))))
    (zeros-string 10)))
