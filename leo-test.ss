(import (micascheme) (leo))

(check (equal? (leo "foo") "foo"))

(check
  (equal?
    (leo
      (use
        ((native string-length (function (length string) number))
         (native number->string (function (string number) string))
         (native string-append (function (append string string) string)))
        (append (string (length "foo")) " apples")))
    "3 apples"))
