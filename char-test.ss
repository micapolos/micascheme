(import (scheme) (check) (char))

(check (equal? (char->ascii #\space) #x20))
(check (equal? (char->ascii (integer->char #xff)) #xff))
(check (equal? (char->ascii (integer->char #x1234)) #x34))
