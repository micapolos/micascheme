(library (typed lang)
  (export
    : assume
    tt
    tt2
    typeof type
    define-typed)
  (import
    (micascheme)
    (typed type)
    (typed typed)
    (typed syntax)
    (typed keywords)
    (typed phased))

  (meta define (type-lookup $lookup)
    (lambda ($identifier)
      ($lookup $identifier #'type)))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed
            (lambda ($type-syntax)
              (eval
                (syntax->datum $type-syntax)
                (environment '(micascheme) '(typed type))))
            (type-lookup $lookup)
            #'expr)))))

  (define-syntax (define-typed $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id expr)
        (identifier? #'id)
        (lets
          ($typed
            (syntax->typed
              (lambda ($type-syntax)
                (eval
                  (syntax->datum $type-syntax)
                  (environment '(micascheme) '(typed type))))
              (type-lookup $lookup)
              #'expr))
          #`(begin
            (define id #,(typed-value $typed))
            (define-property id type #,(type->syntax (typed-type $typed))))))))

  (define-syntax (tt2 $syntax $lookup)
    (define (syntax->phased $lookup $syntax)
      (syntax-case $syntax (assume)
        (x
          (identifier? #'x)
          (lookup-ref $lookup #'x))))
    (define (syntax->typed $lookup $expected-phase $syntax)
      (lets
        ((phased $phase $typed)
          (syntax->phased $lookup $syntax))
        (case
          ((= $phased $expected-phase)
            $typed)
          (else )
            (syntax-error $syntax
              (format "invalid phase ~s, expected ~s in"
                $phase
                $expected-phase)))))
    (lets
      ($typed (syntax->typed $lookup 0 $syntax))
      #`(typed
        (type->syntax (typed-type $typed))
        #,(typed-value $typed))))
)
