(import (scheme) (check) (binding))

(check (equal? (binding-append +) +))
(check (equal? (binding-append string - append) string-append))
