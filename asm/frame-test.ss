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

(check-frame 100
  (fluent (empty-frame)
    (frame+syntax #'(label x)))
  (lambda ()
    (lets
      (x 100)
      (blob 0 (lambda ($port) (void))))))

(check-frame 100
  (fluent (empty-frame)
    (frame+syntax #'(label x))
    (frame+syntax #'(db 10)))
  (lambda ()
    (lets
      (x 100)
      (blob 1
        (lambda ($port)
          (put-db $port 10))))))

(check-frame 100
  (fluent (empty-frame)
    (frame+syntax #'(label x))
    (frame+syntax #'(db 10))
    (frame+syntax #'(db foo)))
  (lambda (foo)
    (lets
      (x 100)
      (blob 2
        (lambda ($port)
          (put-db $port 10)
          (put-db $port foo))))))
