(library (typico library)
  (export
    library library? library-lookup-proc library-exports library-definitions
    empty-library
    library+syntax
    library->datum
    check-library+syntax)
  (import
    (rename
      (micascheme)
      (library %library)
      (library-exports %library-exports))
    (typico id)
    (typico expand)
    (typico type)
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
              ((typed $type $value) (expand-typed $lookup #'expr))
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
                              (typed $type $symbol))
                            (other
                              (expand-typed/no-lookup $lookup #'other))))))))
                (push
                  (library-exports $library)
                  (typed $type $symbol))
                (push
                  (library-definitions $library)
                  `(define ,(id->symbol #'id) ,$value)))))))
      ((function . x)
        (syntax-case #'x ()
          (((id params ...) body)
            (id? #'id)
            (library+syntax $lookup $library
              (syntax->datum/annotation
                #'(value id
                  (lambda (params ...) body)))))))))

  (define (library->datum $library)
    `(library
      (export ,@(map export->datum (reverse (library-exports $library))))
      ,@(reverse (library-definitions $library))))

  (define (export->datum $export)
    `(,(typed-value $export) ,(type->datum (typed-type $export))))

  (define-rule-syntax (check-library+syntax lookup in out)
    (check
      (equal?
        (library->datum (library+syntax lookup (empty-library) (datum/annotation in)))
        'out)))
)
