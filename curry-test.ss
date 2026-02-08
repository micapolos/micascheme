(import (scheme) (check) (curry))

(check (equal? ((curry- 5) 3) 2))
