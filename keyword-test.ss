(import (scheme) (check) (keyword))

(check (equal? (keyword-append here +) +))
(check (equal? (keyword-append here string - append) string-append))

(check (equal? (keyword-replace plus + plus) +))
(check (equal? (keyword-replace plus + (plus 1 2)) 3))
(check (equal? (keyword-replace plus + (plus 1 (plus 2 3))) 6))
