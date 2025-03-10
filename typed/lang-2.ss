(library (typed lang-2)
  (export tt)
  (import
    (micascheme)
    (syntax lookup)
    (any)
    (typed type)
    (typed typed)
    (typed keywords)
    (typed phased))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((id x)
        (let ()
          (define (syntax->phased $lookup $syntax)
            (syntax-case $syntax ()
              (x
                (identifier? #'x)
                (lookup-ref $lookup #'x))
              (x
                (string? (datum x))
                (phased 0 (typed any-string (datum->syntax #'id #'x))))))
          (define (syntax->typed $lookup $expected-phase $syntax)
            (lets
              ((phased $phase $typed)
                (syntax->phased $lookup $syntax))
              (cond
                ((= $phase $expected-phase)
                  $typed)
                (else
                  (syntax-error $syntax
                    (format "invalid phase ~s, expected ~s in"
                      $phase
                      $expected-phase))))))
          (typed-value
            (syntax->typed $lookup 0 #'x))))))
)
