(import (micascheme) (asm frame) (asm parameters) (asm block) (asm program))

(define-rule-syntax (check-frame org actual expected)
  (check-datum=?
    (frame->syntax org actual)
    'expected))

(check-frame 100
  (frame
    (parameters-with a b)
    (program
      (stack
        (cons #'main 10)
        (cons #'test 20))
      (u8-block 1 2)))
  (lambda (a b)
    (lets
      (main 110)
      (test 120)
      (blob 2
        (lambda ($port)
          (put-u8 $port 1)
          (put-u8 $port 2))))))
