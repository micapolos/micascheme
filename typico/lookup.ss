(library (typico lookup)
  (export
    empty-lookup
    lookup+
    lookup+let
    lookup++
    core-lookup)
  (import (micascheme) (typico typed) (typico expand) (typico type) (typico core-types))

  (define (empty-lookup)
    (lambda ($symbol) #f))

  (define (lookup+ $lookup $symbol $value)
    (lambda ($lookup-symbol)
      (cond
        ((symbol=? $lookup-symbol $symbol) $value)
        (else ($lookup $lookup-symbol)))))

  (define (lookup+let $lookup)
    (lookup+ $lookup 'let
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ ((id expr) ...) body)
            (lets
              ($ids (map syntax->datum #'(id ...)))
              ($typed-list (map (partial expand-typed $lookup) #'(expr ...)))
              ($lookup
                (fold-left
                  (lambda ($lookup $id $type)
                    (lookup+ $lookup $id
                      (lambda ($lookup $syntax)
                        (syntax-case $syntax ()
                          (id
                            (symbol? (datum id))
                            (typed $type $id))))))
                  $lookup
                  $ids
                  (map typed-type $typed-list)))
              ($typed-body (expand-typed $lookup #'body))
              (typed
                (typed-type $typed-body)
                `(let
                  (,@(map list
                    (map syntax->datum $ids)
                    (map typed-value $typed-list)))
                  ,(typed-value $typed-body)))))))))

  (define (lookup++ $lookup)
    (lookup+ $lookup '+
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ arg arg* ...)
            (lets
              ($typed-arg (expand-typed $lookup #'arg))
              ($arg-type (typed-type $typed-arg))
              (cond
                ((type=? $arg-type integer-type)
                  (typed integer-type
                    `(($primitive 3 +)
                      ,(typed-value $typed-arg)
                      ,@(map (partial expand-value-of $lookup integer-type) #'(arg* ...)))))
                ((type=? $arg-type string-type)
                  (typed string-type
                    `(($primitive 3 string-append)
                      ,(typed-value $typed-arg)
                      ,@(map (partial expand-value-of $lookup string-type) #'(arg* ...)))))
                (else (syntax-error #'arg
                  "invalid type")))))))))

  (define (core-lookup)
    (fluent (empty-lookup)
      (lookup+let)
      (lookup++)))
)
