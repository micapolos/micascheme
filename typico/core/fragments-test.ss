(import
  (typico base)
  (typico fragment)
  (prefix (typico core fragments) fragment-))

(check (equal? fragment-string-append (fragment (import (scheme)) string-append)))
