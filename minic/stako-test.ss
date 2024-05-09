(import (except (micascheme) switch) (minic stako) (syntax) (syntaxes))

(define-rule-syntax (run-stako $in $op ...)
  (let ()
    (define $val $in)
    (define-rules-syntaxes
      ((io $addr) $val)
      ((io $addr $fx) (set! $val $fx)))
    (stako (1024 io) $op ...)
    $val))

; empty
(check (equal? (run-stako 0) 0))

; out
(check
  (equal?
    (run-stako 0
      (alloc 2)
      (const 1 100)
      (out 1 0)
      (free 1))
    100))

; inc
(check
  (equal?
    (run-stako 100
      (alloc 2)
      (in 1 0)
      (inc 1)
      (out 1 0)
      (free 2))
    101))

; dec
(check
  (equal?
    (run-stako 100
      (alloc 2)
      (in 1 0)
      (dec 1)
      (out 1 0)
      (free 2))
    99))

; add
(check
  (equal?
    (run-stako 100
      (alloc 3)
      (in 1 0)
      (const 2 10)
      (add 1 2)
      (out 1 0)
      (free 3))
    110))

; sub
(check
  (equal?
    (run-stako 100
      (alloc 3)
      (in 1 0)
      (const 2 10)
      (sub 1 2)
      (out 1 0)
      (free 3))
    90))

; block
(check
  (equal?
    (run-stako 0
      (block
        (alloc 2)
        (const 1 10)
        (out 1 0)
        (free 2)))
    10))

; switch
(check
  (equal?
    (run-stako 0
      (alloc 2)
      (in 1 0)
      (switch 1
        (const 1 10)
        (const 1 20))
      (out 1 0)
      (free 2))
    10))

(check
  (equal?
    (run-stako 1
      (alloc 2)
      (in 1 0)
      (switch 1
        (const 1 10)
        (const 1 20))
      (out 1 0)
      (free 2))
    20))

(check
  (equal?
    (run-stako 2
      (alloc 2)
      (in 1 0)
      (switch 1
        (const 1 10)
        (const 1 20))
      (out 1 0)
      (free 2))
    20))

; loop
(check
  (equal?
    (run-stako 35000000
      (alloc 3)
      (in 1 0)
      (const 2 0)
      (loop 1
        (inc 2)
        (inc 2)
        (dec 1))
      (out 2 0)
      (free 3))
    70000000))
