(library (sjasm emit-asm)
  (export emit flush)
  (import (micascheme))

  (define emited '())

  (define (emit $item)
    (cons! $item emited))

  (define (flush)
    (lets
      ($emited emited)
      (run (set! emited '()))
      (reverse $emited)))
)
