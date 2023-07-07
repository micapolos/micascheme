(import (base-syntax))

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))
