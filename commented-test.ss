(import (scheme) (check) (commented))

(check (equal? (commented 123) 123))
(check (equal? (commented nice 123) 123))
(check (equal? (commented (very nice) (and cool) 123) 123))
