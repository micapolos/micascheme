(import (except (micascheme) switch) (minic asm-2) (syntax) (syntaxes))

(define-rule-syntax (test-asm $in $op ...)
  (let ()
    (define $value $in)
    (define-rules-syntaxes
      ((io $addr) $value)
      ((io $addr $fx) (set! $value $fx)))
    (asm (io) $op ...)
    $value))

; empty
(check (equal? (test-asm 0) 0))

; nop
(check (equal? (test-asm 0 (nop)) 0))

; out
(check
  (equal?
    (test-asm 0
      (local 2
        (const 1 100)
        (out 1 0)))
    100))

; inc
(check
  (equal?
    (test-asm 100
      (local 2
        (in 1 0)
        (inc 1)
        (out 1 0)))
    101))

; dec
(check
  (equal?
    (test-asm 100
      (local 2
        (in 1 0)
        (dec 1)
        (out 1 0)))
    99))

; add
(check
  (equal?
    (test-asm 100
      (local 3
        (in 1 0)
        (const 2 10)
        (add 1 2)
        (out 1 0)))
    110))

; sub
(check
  (equal?
    (test-asm 100
      (local 3
        (in 1 0)
        (const 2 10)
        (sub 1 2)
        (out 1 0)))
    90))

; block
(check
  (equal?
    (test-asm 0
      (local 2
        (block
          (const 1 10)
          (out 1 0))))
    10))

; switch
(check
  (equal?
    (test-asm 0
      (local 2
        (in 1 0)
        (switch 1
          (const 1 10)
          (const 1 20))
        (out 1 0)))
    10))

(check
  (equal?
    (test-asm 1
      (local 2
        (in 1 0)
        (switch 1
          (const 1 10)
          (const 1 20))
        (out 1 0)))
    20))

(check
  (equal?
    (test-asm 2
      (local 2
        (in 1 0)
        (switch 1
          (const 1 10)
          (const 1 20))
        (out 1 0)))
    20))

; loop
(check
  (equal?
    (test-asm 35000000
      (local 3
        (in 1 0)
        (const 2 0)
        (loop 1
          (inc 2)
          (inc 2)
          (dec 1))
        (out 2 0)))
    70000000))
