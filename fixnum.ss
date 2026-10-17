(library (fixnum)
  (export
    fx+1/wraparound
    fx-1/wraparound

    fx+1/wraparound!
    fx-1/wraparound!
    fx+/wraparound!
    fx-/wraparound!
    fxsll!
    fxsrl!

    fxmask
    fxbits
    fxpack)
  (import
    (scheme)
    (syntax)
    (syntaxes))

  (define (fx+1/wraparound x)
    (fx+/wraparound x 1))

  (define (fx-1/wraparound x)
    (fx-/wraparound x 1))

  (define-rule-syntax (fx+1/wraparound! fx)
    (fx+/wraparound! fx 1))

  (define-rule-syntax (fx-1/wraparound! fx)
    (fx-/wraparound! fx 1))

  (define-rule-syntax (fx+/wraparound! fx x)
    (set! fx (fx+/wraparound fx x)))

  (define-rule-syntax (fx-/wraparound! fx x)
    (set! fx (fx-/wraparound fx x)))

  (define-rule-syntax (fxsll! fx n)
    (set! fx (fxsll fx n)))

  (define-rule-syntax (fxsrl! fx n)
    (set! fx (fxsrl fx n)))

  (define-rule-syntax (fxmask bits)
    (fx- (fxsll 1 bits) 1))

  (define-rule-syntax (fxbits bits fx)
    (fxand (fxmask bits) fx))

  (define-rules-syntax
    ((fxpack) 0)
    ((fxpack (bits fx))
      (fxbits bits fx))
    ((fxpack (bits fx) (bits2 fx2) . tail)
      (fxpack
        (
          (fx+/wraparound bits bits2)
          (fxior
            (fxsll fx bits2)
            (fxbits bits2 fx2)))
        . tail)))
)
