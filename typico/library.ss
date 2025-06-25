(library (typico library)
  (export
    library library? library-lookup-proc library-exports library-definitions
    empty-library
    library+syntax
    check-library+syntax)
  (import
    (rename
      (micascheme)
      (library %library)
      (library-exports %library-exports))
    (typico id)
    (typico expand)
    (typico typed)
    (typico lookup))

  (data (library lookup-proc exports definitions))

  (define (empty-library)
    (library identity (stack) (stack)))

  (define (library+syntax $lookup $library $syntax)
    (syntax-case $syntax (value function)
      ((value . x)
        (syntax-case #'x ()
          ((id expr)
            (id? #'id)
            (lets
              ($symbol (id->symbol #'id))
              ($typed (expand-typed $lookup #'expr))
              (library
                (lambda ($lookup)
                  (cond
                    (($lookup $symbol)
                      (syntax-error #'id "already defined"))
                    (else
                      (lookup+ $lookup $symbol
                        (lambda ($lookup $syntax)
                          (syntax-case $syntax ()
                            (id
                              (id? #'id)
                              (typed (typed-type $typed) $symbol))
                            (other
                              (expand-typed/no-lookup $lookup #'other))))))))
                (push (library-exports $library) $symbol)
                (push
                  (library-definitions $library)
                  `(define
                    ,(id->symbol #'id)
                    ,(typed-value $typed))))))))
      ((function . x)
        (syntax-case #'x ()
          (((id params ...) body)
            (id? #'id)
            (library+syntax $lookup $library
              (syntax->datum/annotation
                #'(value id
                  (lambda (params ...) body)))))))))

  (define-rule-syntax (library)
    (check-library+syntax lookup in (library define ...) (id type) ...)
    (lets
      ($library (library+syntax lookup (empty-library) (datum/annotation in)))
      (run
        (check
          (equal?
            (reverse (library-definitions $library))
            '(define ...)))
        (check
          (equal?
            (lets
              ($proc (library-lookup-proc $library))
              ($lookup ($proc (empty-lookup)))
              ($expander ($lookup 'id))
              (run (check (not (false? $expander))))
              ($typed ($expander $lookup (datum/annotation id)))
              (typed->datum $typed))
            '(typed type id))) ...)))
)
