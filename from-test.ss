(import (check) (from))

(check (equal? (from (from-testing) foo) "foo"))
(check (equal? (from (from-testing) single?) "single? override"))
