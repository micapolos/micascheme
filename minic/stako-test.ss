(import (micascheme) (minic stako))

(check
  (equal?
    (stako 0 (bytevector 10 20))
    (bytevector 10 20)))

; alloc / free
(check
  (equal?
    (stako 1 (bytevector 10)
      (alloc 1)
      (const-8 0 20))
    (bytevector 20 10)))

(check
  (equal?
    (stako 0 (bytevector 10 20 30)
      (free 2))
    (bytevector 30)))

; math-8
(check (equal? (stako 0 (bytevector 20 30) (inc-8 1)) (bytevector 20 31)))
(check (equal? (stako 0 (bytevector 20 30) (dec-8 1)) (bytevector 20 29)))
(check (equal? (stako 0 (bytevector 20 30) (add-8 1 0)) (bytevector 20 50)))
(check (equal? (stako 0 (bytevector 20 30) (sub-8 1 0)) (bytevector 20 10)))

; block
(check (equal? (stako 1 (bytevector 20 30) (block (alloc 1) (const-8 0 10))) (bytevector 10 20 30)))

; switch-8
(check (equal? (stako 0 (bytevector 0) (switch-8 0 (const-8 0 10) (const-8 0 20) (const-8 0 30))) (bytevector 10)))
(check (equal? (stako 0 (bytevector 1) (switch-8 0 (const-8 0 10) (const-8 0 20) (const-8 0 30))) (bytevector 20)))
(check (equal? (stako 0 (bytevector 2) (switch-8 0 (const-8 0 10) (const-8 0 20) (const-8 0 30))) (bytevector 30)))
(check (equal? (stako 0 (bytevector 3) (switch-8 0 (const-8 0 10) (const-8 0 20) (const-8 0 30))) (bytevector 30)))
