(import (micascheme) (asm fragment) (asm block) (syntax lookup))

(check
  (equal?
    (fragment->datum
      (fragment-with (foo bar)
        (block-with (2 $port)
          (put-u8 $port 10)
          (put-u8 $port 20))))
    '(fragment (foo bar)
      (blob 2
        (lambda ($port)
          (put-u8 $port 10)
          (put-u8 $port 20))))))

(check
  (equal?
    (fragment->datum
      (fragment-append
        (fragment-with (foo bar)
          (block-with (2 $port)
            (put-u8 $port 10)
            (put-u8 $port 20)))
        (fragment-with (bar gar)
          (block-with (2 $port)
            (put-u8 $port 30)
            (put-u8 $port 40)))))
    '(fragment (bar foo gar)
      (blob 4
        (lambda ($port)
          (put-u8 $port 10)
          (put-u8 $port 20)
          (put-u8 $port 30)
          (put-u8 $port 40))))))

(check
  (equal?
    (fragment->datum (syntax->fragment #`(db 10 foo 30)))
    '(fragment (foo)
      (blob 3
        (lambda ($port)
          (put-u8 $port 10)
          (put-u8 $port foo)
          (put-u8 $port 30))))))

