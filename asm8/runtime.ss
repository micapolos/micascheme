(library (asm8 runtime)
  (export
    u8= u8+ u8- u8*
    u16= u16+ u16-
    u8-ref u8-set!

    sf?
    zf?
    pv?
    cf?
    scf!)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets))
  (export (import (values)))

  (define memory-bytevector (make-bytevector #x4000))

  (define (u8-ref $addr)
    (bytevector-u8-ref memory-bytevector (fxand $addr #x3fff)))

  (define (u8-set! $addr $u8)
    (bytevector-u8-set! memory-bytevector (fxand $addr #x3fff) $u8))

  (define flags-box (box #x00))

  (define (flags) (unbox flags-box))
  (define (set-flags! $flags) (set-box! flags-box $flags))

  (define (sf?) (not (zero? (fxand (flags) #b10000000))))
  (define (zf?) (not (zero? (fxand (flags) #b01000000))))
  (define (pv?) (not (zero? (fxand (flags) #b00000100))))
  (define (cf?) (not (zero? (fxand (flags) #b00000001))))

  (define (scf!) (set-flags! (fxior (flags) #b00000001)))

  (define-rules-syntaxes
    (u8-false 0)
    (u8-true 1)

    ((u8-result x)
      (fxand x #xff))
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
      (u8-eqz (fxxor x y)))

    ((u16-result x)
      (fxand x #xffff))
    ((u16-unop op x)
      (u16-result (op x)))
    ((u16-binop op x y)
      (u16-result (op x y)))
    ((u16+ x y)
      (u8-binop fx+/wraparound x y))
    ((u16- x y)
      (u8-binop fx-/wraparound x y))
    ((u16-zero? x)
      (if (zero? x) u16-true u16-false))
    ((u16= x y)
      (u16-eqz (fxxor x y))))

  (define (u8+/wraparound $x $y)
    (fxand #xff (fx+/wraparound $x $y)))

  (define (u8+/carry? $carry? $x $y)
    (lets
      ($result9 (fx+/wraparound $x $y))
      ($result8 (fxand $result9 #xff))
      ($carry? (not (zero? (fxand #x100))))
      (values $carry? $result8)))
)
