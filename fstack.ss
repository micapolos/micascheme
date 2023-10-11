(library (fstack)
  (export
    fstack-run
    fstack-block
    fstack-define)
  (import (micascheme))

  (define fstack-address-parameter
    (make-thread-parameter #f))

  (define fstack-top-parameter
    (make-thread-parameter #f))

  (define-syntax-rule (fstack-run $size $body ...)
    (lets
      ($address (foreign-alloc $size))
      (parameterize
        ((fstack-address-parameter $address)
         (fstack-top-parameter (+ $address $size)))
        (dynamic-wind
          (lambda () (void))
          (lambda () $body ...)
          (lambda () (foreign-free $address))))))

  (define-syntax-rule (fstack-block $fstack $body ...)
    (parameterize ((fstack-address-parameter (fstack-address-parameter)))
      $body ...))

  (define-syntax-rule (fstack-define $ftype $var)
    (define $var
      (lets
        ($pointer (make-ftype-pointer $ftype (fstack-address-parameter)))
        (do (fstack-address-parameter (+ (fstack-address-parameter) (ftype-sizeof $ftype))))
        (if (> (fstack-address-parameter) (fstack-top-parameter))
          (error `fstack-define "fstack overflow"))
        $pointer)))
)
