(import (micascheme) (asm fragment) (asm block) (syntax lookup))

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

(define fragment-lookup
  (lookup-with
    (fragment-1 fragment-1)
    (fragment-2 fragment-2)
    (fragment-3 fragment-3)
    (main main)))

(check-datum=?
  (org-label->put-proc-syntax fragment-lookup 100 #'main)
  '(lets
    (fragment-2 100)
    (fragment-1 102)
    (fragment-3 103)
    (main 106)
    (lambda ($port)
      (put-u8 $port 20)
      (put-u8 $port 30)
      (put-u8 $port 10)
      (put-u8 $port 40)
      (put-u8 $port 50)
      (put-u8 $port 60)
      (put-u8 $port 70))))
