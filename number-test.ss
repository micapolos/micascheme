(import (check) (number))

(check (nonnegative-integer? 0))
(check (nonnegative-integer? 1))
(check (not (nonnegative-integer? -1)))
(check (not (nonnegative-integer? -2)))
(check (not (nonnegative-integer? 1.1)))

(let (($fn (lambda (s) (string-append s "!"))))
  (check (equal? (iterate $fn "Hello" 0) "Hello"))
  (check (equal? (iterate $fn "Hello" 3) "Hello!!!")))
