(library (tt lang-macros)
  (export
    compile-and
    compile-or)
  (import
    (scheme)
    (lets)
    (procedure)
    (tt hoas)
    (tt primitive)
    (tt type)
    (tt hoas-compiler))

  (define (compile-and $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($xs (map (partial compile-value $lookup boolean-type) #'(x ...)))
          (typed
            boolean-type
            #`(and #,@$xs))))))

  (define (compile-or $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($xs (map (partial compile-value $lookup boolean-type) #'(x ...)))
          (typed
            boolean-type
            #`(or #,@$xs))))))

  ; (define (compile-partial $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ proc param ...)
  ;       (lets
  ;         ((typed $arrow $proc)
)
