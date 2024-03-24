(library (tico tuple)
  (export tuple tuple-ref)
  (import (micascheme))

  (define-syntax (tuple $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (lets
          ($items (syntax->list #'($item ...)))
          (case (length $items)
            ((0) #'#f)
            ((1) (car $items))
            ((2) #`(cons #,(car $items) #,(cadr $items)))
            (else #`(vector #,@$items)))))))

  (define-syntax (tuple-ref $syntax)
    (syntax-case $syntax ()
      ((_ $arity $tuple $index)
        (and
          (nonnegative-integer? (syntax->datum #'$arity))
          (nonnegative-integer? (syntax->datum #'$index)))
        (lets
          ($arity (syntax->datum #'$arity))
          ($index (syntax->datum #'$index))
          (case $arity
            ((0) #'#f)
            ((1) #'$tuple)
            ((2) #`(#,(if (zero? $index) #`car #`cdr) $tuple))
            (else #`(vector-ref $tuple #,$index)))))))
)
