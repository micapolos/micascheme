(import (micascheme) (asm fragment) (asm program) (asm block) (syntax lookup))

; === program->syntax ===

(define fragment-1
  (fragment
    '()
    (u8-block 10)))

(define fragment-2
  (fragment
    '()
    (u8-block 20 30)))

(define fragment-3
  (fragment
    (list #'fragment-2 #'fragment-1)
    (u8-block 40 50 60)))

(define main
  (fragment
    (list #'fragment-3 #'fragment-1)
    (u8-block 70)))

(define lookup
  (lookup-with
    (fragment-1 fragment-1)
    (fragment-2 fragment-2)
    (fragment-3 fragment-3)
    (main main)))

(check-datum=?
  (program->syntax #x2000 (label->program lookup #'main))
  '(lets
    (main 8192)
    (fragment-3 8193)
    (fragment-2 8196)
    (fragment-1 8198)
    (blob 7
      (lambda ($port)
        (put-u8 $port 70)
        (put-u8 $port 40)
        (put-u8 $port 50)
        (put-u8 $port 60)
        (put-u8 $port 20)
        (put-u8 $port 30)
        (put-u8 $port 10)))))
