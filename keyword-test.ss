(import (scheme) (check) (keyword))

(check (equal? (keyword-append +) +))
(check (equal? (keyword-append string - append) string-append))

(check (equal? (keyword-replace plus + plus) +))
(check (equal? (keyword-replace plus + (plus 1 2)) 3))
(check (equal? (keyword-replace plus + (plus 1 (plus 2 3))) 6))
