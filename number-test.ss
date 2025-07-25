(import (scheme) (check) (number) (syntax))

(check (nonnegative-integer? 0))
(check (nonnegative-integer? 1))
(check (not (nonnegative-integer? -1)))
(check (not (nonnegative-integer? -2)))
(check (not (nonnegative-integer? 1.1)))

(let (($fn (lambda (s) (string-append s "!"))))
  (check (equal? (iterate $fn "Hello" 0) "Hello"))
  (check (equal? (iterate $fn "Hello" 3) "Hello!!!")))

(check (one? 1))
(check (not (one? 0)))

(check
  (equal?
    (iterate-indexed
      (lambda ($list $index)
        (cons (* $index 10) $list))
      (list 'end)
      5)
    (list 40 30 20 10 0 'end)))

(check (equal? (bitwise-align #x1234 1) #x1234))
(check (equal? (bitwise-align #x1235 1) #x1235))

(check (equal? (bitwise-align #x1234 2) #x1234))
(check (equal? (bitwise-align #x1235 2) #x1236))
(check (equal? (bitwise-align #x1236 2) #x1236))
(check (equal? (bitwise-align #x1237 2) #x1238))

(check (equal? (bitwise-align #x1234 4) #x1234))
(check (equal? (bitwise-align #x1235 4) #x1238))
(check (equal? (bitwise-align #x1236 4) #x1238))
(check (equal? (bitwise-align #x1237 4) #x1238))
(check (equal? (bitwise-align #x1238 4) #x1238))
(check (equal? (bitwise-align #x1239 4) #x123c))

(check (bitwise-aligned? #x1234 4))
(check (not (bitwise-aligned? #x1235 4)))
