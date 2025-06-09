(import (micascheme) (asm fragment) (asm block) (syntax lookup))

(check-datum=?
  (fragment->syntax
    (fragment-with (foo bar)
      (block-with ($port 2)
        (put-u8 $port 10)
        (put-u8 $port 20))))
  '(fragment (foo bar)
    (blob 2
      (lambda ($port)
        (put-u8 $port 10)
        (put-u8 $port 20)))))

(check-datum=?
  (fragment->syntax
    (fragment-append
      (fragment-with (foo bar)
        (block-with ($port 2)
          (put-u8 $port 10)
          (put-u8 $port 20)))
      (fragment-with (bar gar)
        (block-with ($port 2)
          (put-u8 $port 30)
          (put-u8 $port 40)))))
  '(fragment (foo bar gar)
    (blob 4
      (lambda ($port)
        (put-u8 $port 10)
        (put-u8 $port 20)
        (put-u8 $port 30)
        (put-u8 $port 40)))))

(check
  (equal?
    (fragment->datum (syntax->fragment #`(db 10 foo 30 "foo")))
    '(fragment (foo)
      (blob 4
        (lambda ($port)
          (put-db $port 10)
          (put-db $port foo)
          (put-db $port 30)
          (put-db $port "foo"))))))

(check
  (equal?
    (fragment->datum (syntax->fragment #`(dw #x1234 #x2345)))
    '(fragment ()
      (blob 4
        (lambda ($port)
          (put-dw $port #x1234)
          (put-dw $port #x2345))))))

