(import (asm-3 base) (asm-2 aligned) (asm-3 sized) (asm-2 aligned-sized))

(define (test-slack $size)
  (make-string $size #\.))

(check
  (equal?
    (aligned-sized-append test-slack string-append)
    (aligned 1 (sized 0 ""))))

(check
  (equal?
    (aligned-sized-append test-slack string-append
      (aligned 4 (sized 3 "foo")))
    (aligned 4 (sized 3 "foo"))))

(check
  (equal?
    (aligned-sized-append test-slack string-append
      (aligned 4 (sized 8 "12345678"))
      (aligned 2 (sized 1 "!")))
    (aligned 4 (sized 9 "12345678!"))))

(check
  (equal?
    (aligned-sized-append test-slack string-append
      (aligned 4 (sized 5 "12345"))
      (aligned 2 (sized 1 "!")))
    (aligned 4 (sized 9 "12345...!"))))

(check
  (equal?
    (aligned-sized-append test-slack string-append
      (aligned 4 (sized 5 "12345"))
      (aligned 1 (sized 3 "123"))
      (aligned 8 (sized 5 "12345"))
      (aligned 1 (sized 1 "1"))
      (aligned 1 (sized 1 "!")))
    (aligned 8 (sized 33 "12345...123.....12345...1.......!"))))
