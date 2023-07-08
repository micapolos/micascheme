(library (gen)
  (export 
    gen 
    gen-value state-gen
    gen-bind! gen-map!
    gen-map-state gen-map-state!)

  (import (micascheme))

  (data (stateful state value))

  (define (gen $value)
    (lambda ($state)
      (stateful $state $value)))

  (define (gen-value $gen $state)
    (stateful-value ($gen $state)))

  (define state-gen
    (lambda ($state)
      (stateful $state $state)))

  (define (gen-bind $gen $fn)
    (lambda ($state)
      (bind ($stateful ($gen $state))
        (($fn (stateful-value $stateful)) (stateful-state $stateful)))))

  (define-syntax gen-bind!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var gen) body ...)
          #`(gen-bind gen
            (lambda (var) body ...))))))
  
  (define (gen-map $gen $fn)
    (gen-bind! ($value $gen) 
      (gen ($fn $value))))

  (define-syntax gen-map!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var gen) body ...)
          #`(gen-map gen
            (lambda (var) body ...))))))

  (define (gen-map-state $gen $fn)
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var gen) body ...)
          #`(gen-map-state gen
            (lambda (var) body ...))))))

  (define-syntax gen-map-state!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var gen) body ...)
          #`(gen-map-state gen
            (lambda (var) body ...))))))
)