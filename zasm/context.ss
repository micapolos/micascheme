(library (zasm context)
  (export
    make-context
    context?
    context-org
    context-bytevector
    context-org-set!
    context-db!
    context-dw!
    context=?)
  (import (micascheme))

  (define-record-type context
    (fields
      (mutable org)
      (immutable bytevector)))

  (define (context=? $context-1 $context-2)
    (and
      (= (context-org $context-1) (context-org $context-2))
      (bytevector=? (context-bytevector $context-1) (context-bytevector $context-2))))

  (define (context-db! $context $u8)
    (lets
      ($org (context-org $context))
      (run
        (bytevector-u8-set! (context-bytevector $context) $org $u8))
        (context-org-set! $context (+ $org 1))))

  (define (context-dw! $context $u16)
    (lets
      ($org (context-org $context))
      (run
        (bytevector-u16-set! (context-bytevector $context) $org $u16 'little)
        (context-org-set! $context (+ $org 2)))))

  (record-writer (record-type-descriptor context)
    (lambda ($context $port $write)
      (display "(context " $port)
      ($write (context-org $context) $port)
      (display " " $port)
      ($write (context-bytevector $context) $port)
      (display ")" $port)))
)
