(import (scheme) (check) (procedure-name))

(check (equal? (procedure-name? +) '+))
(check (equal? (procedure-name? string-append) 'string-append))
(check (equal? (procedure-name? (lambda x x)) #f))
