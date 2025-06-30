(import
  (typico base)
  (typico fragment)
  (prefix (typico core fragments) %))

(check (equal? %string-append (fragment (import (scheme)) string-append)))
