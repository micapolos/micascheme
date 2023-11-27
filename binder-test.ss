(import (check) (binder) (lets))

(define-binder (string-data string->number string-length string->list))

(lets
  ((string-data $number $length $chars) "123")
  (do (check (equal? $number 123)))
  (do (check (equal? $length 3)))
  (do (check (equal? $chars (list #\1 #\2 #\3))))
  (void))
