(import (micascheme) (leo))

(check (equal? (leo "foo") "foo"))

(check
  (equal?
    (leo
      (use
        ((native string-length (arrow (length string) number))
         (native number->string (arrow (string number) string))
         (native string-append (arrow (append string string) string)))
        (append (string (length "foo")) " apples")))
    "3 apples"))
