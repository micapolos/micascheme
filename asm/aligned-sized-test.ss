(import (asm base) (asm aligned) (asm sized) (asm aligned-sized))

(define (test-slack $size)
  (make-string $size #\.))

(check
  (equal?
    (aligned-sized-append test-slack)
    (aligned 1 (list))))

(check
  (equal?
    (aligned-sized-append test-slack
      (aligned 4 (sized 3 "foo")))
    (aligned 4
      (list
        (sized 3 "foo")))))

(check
  (equal?
    (aligned-sized-append test-slack
      (aligned 4 (sized 8 "12345678"))
      (aligned 2 (sized 1 "!")))
    (aligned 4
      (list
        (sized 8 "12345678")
        (sized 1 "!")))))

(check
  (equal?
    (aligned-sized-append test-slack
      (aligned 4 (sized 5 "12345"))
      (aligned 2 (sized 1 "!")))
    (aligned 4
      (list
        (sized 5 "12345")
        (sized 3 "...")
        (sized 1 "!")))))

(check
  (equal?
    (aligned-sized-append test-slack
      (aligned 4 (sized 5 "12345"))
      (aligned 1 (sized 3 "123"))
      (aligned 8 (sized 5 "12345"))
      (aligned 1 (sized 1 "1"))
      (aligned 1 (sized 1 "!")))
    (aligned 8
      (list
        (sized 5 "12345")
        (sized 3 "...")
        (sized 3 "123")
        (sized 5 ".....")
        (sized 5 "12345")
        (sized 3 "...")
        (sized 1 "1")
        (sized 7 ".......")
        (sized 1 "!")))))

