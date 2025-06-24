(import (typico lang))

(check-equal? 123 123)
(check-equal? (let () 123) 123)
(check-equal? (let ((x 10)) x) 10)
