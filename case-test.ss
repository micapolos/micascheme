(import (check) (case))

(check (equal? (case1 1 (1 "one") (2 "two")) "one"))
(check (equal? (case1 2 (1 "one") (2 "two")) "two"))
(check (equal? (case1 3 (1 "one") (2 "two")) (void)))
(check (equal? (case1 3 (1 "one") (2 "two") (else "other")) "other"))
