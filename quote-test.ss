(import (scheme) (check) (quote))

(check
  (equal?
    (quote-operator
      (string-append (string-append "a" "b") "c"))
    '(string-append "ab" "c")))
