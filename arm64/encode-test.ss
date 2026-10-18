(import (scheme) (arm64 encode) (check))

(check (equal? (size-64? #xffffffff) #f))
(check (equal? (size-64? #x100000000) #t))
