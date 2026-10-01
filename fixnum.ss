(library (fixnum)
  (export
    fx+1/wraparound
    fx-1/wraparound

    fx+1/wraparound!
    fx-1/wraparound!
    fx+/wraparound!
    fx-/wraparound!
    fxsll!
    fxsrl!)
  (import
    (scheme)
    (syntax))

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
)
