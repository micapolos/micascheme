(library (zasm keywords)
  (export
    zasm-bytevector
    check-zasm-bytevector
    unreachable
    nop
    u8.const
    (rename
      (%if if)
      (%else else)))
  (import (micascheme))

  (define-syntax (zasm-bytevector $syntax $lookup)
    (syntax-case $syntax ()
      ((_ op ...)
        (bytevector->syntax
          (binary->bytevector
            (list->binary
              (map
                (lambda ($op)
                  (syntax-case $op ()
                    ((id . args)
                      (identifier? #'id)
                      (lets
                        ($syntax->binary
                          (or
                            ($lookup #'id)
                            (syntax-error #'id "invalid zasm keyword")))
                        ($syntax->binary $syntax->binary $op)))))
                #'(op ...))))))))

  (define-keyword %else)

  (define-syntax unreachable
    (make-compile-time-value
      (lambda ($syntax->binary $syntax)
        (syntax-case $syntax ()
          ((_) (u8-binary #x00))))))

  (define-syntax nop
    (make-compile-time-value
      (lambda ($syntax->binary $syntax)
        (syntax-case $syntax ()
          ((_) (u8-binary #x01))))))

  (define-syntax u8.const
    (make-compile-time-value
      (lambda ($syntax->binary $syntax)
        (syntax-case $syntax ()
          ((_ n)
            (binary-append
              (u8-binary #x02)
              (u8-binary (datum n))))))))

  (define-syntax %if
    (make-compile-time-value
      (lambda ($transform $syntax)
        (syntax-case $syntax (%else)
          ((_ expr x ... (else y ...))
            (lets
              ($x-puts (map (partial $transform $transform) #'(x ...)))
              ($y-puts (map (partial $transform $transform) #'(y ...)))
              (lambda ($port)
                (put-u8 $port #x04)
                (for-each (lambda ($put) ($put $port)) $x-puts)
                (put-u8 $port #x05)
                (for-each (lambda ($put) ($put $port)) $y-puts))))
          ((_ expr x ...)
            (lets
              ($puts (map (partial $transform $transform) #'(x ...)))
              (lambda ($port)
                (put-u8 $port #x04)
                (for-each (lambda ($put) ($put $port)) $puts))))))))

  (define-rule-syntax (check-zasm-bytevector op ... (b ...))
    (check (equal? (zasm-bytevector op ...) (bytevector b ...))))
)
