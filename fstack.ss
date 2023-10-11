(library (fstack)
  (export
    frun
    fblock
    flocal
    flambda
    fdefine)
  (import (micascheme))

  (define fstack-address-parameter
    (make-thread-parameter #f))

  (define fstack-top-parameter
    (make-thread-parameter #f))

  (define-syntax-rule (frun $size $body ...)
    (lets
      ($address (foreign-alloc $size))
      (parameterize
        ((fstack-address-parameter $address)
         (fstack-top-parameter (+ $address $size)))
        (dynamic-wind
          (lambda () (void))
          (lambda () $body ...)
          (lambda () (foreign-free $address))))))

  (define-syntax-rule (fblock $body ...)
    (parameterize ((fstack-address-parameter (fstack-address-parameter)))
      $body ...))

  (define-syntax-rule (flambda ($param ...) $body ...)
    (lambda ($param ...)
      (fblock $body ...)))

  (define-syntax-rule (fdefine ($name $param ...) $body ...)
    (define $name
      (flambda ($param ...) $body ...)))

  (define-syntax-rule (flocal $ftype $var)
    (define $var
      (lets
        ($pointer (make-ftype-pointer $ftype (fstack-address-parameter)))
        (do (fstack-address-parameter (+ (fstack-address-parameter) (ftype-sizeof $ftype))))
        (if (> (fstack-address-parameter) (fstack-top-parameter))
          (error `fdefine "fstack overflow"))
        $pointer)))
)
