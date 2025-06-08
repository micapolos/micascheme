(import (micascheme) (asm fragment) (asm block) (syntax lookup))

(check
  (equal?
    (fragment->datum (syntax->fragment #`(db 10 foo 30)))
    '(fragment (foo)
      (blob 3
        (lambda ($port)
          (put-u8 $port 10)
          (put-u8 $port foo)
          (put-u8 $port 30))))))

