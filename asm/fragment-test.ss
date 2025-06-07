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
  (org-label->put-proc-syntax fragment-lookup #'main)
  '(lambda ($port $org)
    (lets
      (main (+ $org 0))
      (fragment-3 (+ $org 1))
      (fragment-2 (+ $org 4))
      (fragment-1 (+ $org 6))
      (run
        (put-u8 $port 70)
        (put-u8 $port 40)
        (put-u8 $port 50)
        (put-u8 $port 60)
        (put-u8 $port 20)
        (put-u8 $port 30)
        (put-u8 $port 10)))))
