(library (asm8 runtime)
  (export
    u8= u8+ u8- u8*
    u8-ref u8-set!)
  (import
    (scheme)
    (syntax)
    (syntaxes))

  (define memory-bytevector (make-bytevector #x4000))

  (define (u8-ref $addr)
    (bytevector-u8-ref memory-bytevector (fxand $addr #x3fff)))

  (define (u8-set! $addr $u8)
    (bytevector-u8-set! memory-bytevector (fxand $addr #x3fff) $u8))

  (define-rules-syntaxes
    (u8-false 0)
    (u8-true 1)
    ((u8-result x)
      (fxand #xff))
    ((u8-unop op x)
      (u8-result (op x)))
    ((u8-binop op x y)
      (u8-result (op x y)))
    ((u8+ x y)
      (u8-binop fx+/wraparound x y))
    ((u8- x y)
      (u8-binop fx-/wraparound x y))
    ((u8* x y)
      (u8-binop fx*/wraparound x y))
    ((u8-eqz x)
      (if (zero? x) u8-true u8-false))
    ((u8= x y)
      (u8-eqz (fxxor x y))))
)
