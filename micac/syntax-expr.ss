(library (micac syntax-expr)
  (export fold-constants)
  (import (micascheme))

  (define (fold-constants $default $expr)
    (syntax-case $expr (+)
      ((op arg ...)
        (lets
          ($fn
            (syntax-case #'op (+ - and or xor)
              (+ +)
              (and bitwise-and)
              (or bitwise-ior)
              (xor bitwise-xor)
              (_ #f)))
          (or
            (and $fn
              (lets
                ((pair $acc $exprs)
                  (fold-right
                    (lambda ($arg $acc-exprs)
                      (lets
                        ((pair $acc $exprs) $acc-exprs)
                        (switch (syntax->datum $arg)
                          ((number? $arg-number)
                            (pair
                              (if $acc ($fn $acc $arg-number) $arg-number)
                              $exprs))
                          ((else _)
                            (pair
                              $acc
                              (cons ($default $arg) $exprs))))))
                    (pair #f (list))
                    (syntax->list #'(arg ...))))
                (cond
                  ((not $acc) #`#,($fn))
                  ((null? $exprs) #`#,$acc)
                  (else #`(op #,$acc #,@$exprs)))))
              $expr)))))
)
