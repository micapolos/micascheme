(library (react-lib)
  (export noise message osc rect audio mouse-x mouse-y space? frames mix sine square pulse triangle make)
  (import (micascheme))

  (define-aux-keyword noise)
  (define-aux-keyword rect)
  (define-aux-keyword message)
  (define-aux-keyword audio)
  (define-aux-keyword osc)
  (define-aux-keyword mouse-x)
  (define-aux-keyword mouse-y)
  (define-aux-keyword space?)
  (define-aux-keyword frames)
  (define-aux-keyword make)

  (define (mix . $values)
    (/ (apply + $values) (length $values)))

  (define (sine $x)
    (* 0.5 (+ (sin (* $x pi2)) 1)))

  (define (square $x)
    (pulse $x 0.5))

  (define (triangle $x)
    (if (< $x 0.5)
      (* 2 $x)
      (- 2 (* 2 $x))))

  (define (pulse $x $w)
    (if (< $x $w) 0 1))
)
