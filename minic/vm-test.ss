(import (except (micascheme) switch) (minic vm) (minic vm-keywords) (syntax) (syntaxes))

(define-rule-syntax (run-vm $in $op ...)
  (let ()
    (define $fxvector (make-fxvector 1024))
    (define $sp (fxvector-length $fxvector))
    (define $value $in)
    (define-rules-syntaxes
      ((io $addr) $value)
      ((io $addr $fx) (set! $value $fx)))
    (vm ($fxvector $sp io) $op ...)
    $value))

; empty
(check (equal? (run-vm 0) 0))

; out
(check
  (equal?
    (run-vm 0
      (alloc 2)
      (const 1 100)
      (out 1 0)
      (free 1))
    100))

; inc
(check
  (equal?
    (run-vm 100
      (alloc 2)
      (in 1 0)
      (inc 1)
      (out 1 0)
      (free 2))
    101))

; dec
(check
  (equal?
    (run-vm 100
      (alloc 2)
      (in 1 0)
      (dec 1)
      (out 1 0)
      (free 2))
    99))

; add
(check
  (equal?
    (run-vm 100
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
    (run-vm 100
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
    (run-vm 0
      (block
        (alloc 2)
        (const 1 10)
        (out 1 0)
        (free 2)))
    10))

; switch
(check
  (equal?
    (run-vm 0
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
    (run-vm 1
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
    (run-vm 2
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
    (run-vm 35000000
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
