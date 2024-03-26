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

  (define-rule-syntax (frun $size $body ...)
    (lets
      ($address (foreign-alloc $size))
      (parameterize
        ((fstack-address-parameter $address)
         (fstack-top-parameter (+ $address $size)))
        (dynamic-wind
          (lambda () (void))
          (lambda () $body ...)
          (lambda () (foreign-free $address))))))

  (define-rule-syntax (fblock $body ...)
    (parameterize ((fstack-address-parameter (fstack-address-parameter)))
      $body ...))

  (define-rule-syntax (flambda ($param ...) $body ...)
    (lambda ($param ...)
      (fblock $body ...)))

  (define-rule-syntax (fdefine ($name $param ...) $body ...)
    (define $name
      (flambda ($param ...) $body ...)))

  (define-rule-syntax (flocal $ftype $var)
    (define $var
      (let (($pointer (make-ftype-pointer $ftype (fstack-address-parameter))))
        (fstack-address-parameter (+ (fstack-address-parameter) (ftype-sizeof $ftype)))
        (if (> (fstack-address-parameter) (fstack-top-parameter)) (error `flocal "fstack overflow"))
        $pointer)))
)
