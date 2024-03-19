(library (labs syntax)
  (export
    syntax-cons
    syntax-flatten
    flat-map-syntax
    flat-map-syntax-list)
  (import (micascheme))

  (define (syntax-cons $car $cdr)
    #`(#,$car . #,$cdr))

  (define (syntax-flatten $syntax)
    (syntax-case $syntax (begin)
      ((begin $body ...)
        (syntax->list #'($body ...)))
      ($other
        (list #'$other))))

  (define (flat-map-syntax $fn $syntax)
    (syntax-case $syntax (begin)
      ((begin $body ...)
        (flat-map-syntax-list $fn
          (syntax->list #'($body ...))))
      ($other
        (list ($fn #'$other)))))

  (define (flat-map-syntax-list $fn $syntax-list)
    (flatten (map (partial flat-map-syntax $fn) $syntax-list)))
)
