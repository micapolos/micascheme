(library (typico core lookup)
  (export core-lookup)
  (import (micascheme) (typico typed) (typico expand) (typico type) (typico core types) (typico lookup))

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

  (define-rule-syntax (lookup+primitive $lookup in type out)
    (lookup+ $lookup 'in
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (typed
              type
              `($primitive 3 out)))
          (other
            (expand-typed/no-lookup $lookup #'other))))))

  (define (lookup+integer+ $lookup)
    (lookup+ $lookup 'integer+
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (typed
              (function-type (list* integer-type) integer-type)
              `($primitive 3 +)))
          (other
            (expand-typed/no-lookup $lookup #'other))))))

  (define (lookup+string+ $lookup)
    (lookup+ $lookup 'string+
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (typed
              (function-type (list* string-type) string-type)
              `($primitive 3 string-append)))
          (other
            (expand-typed/no-lookup $lookup #'other))))))

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
                (else
                  (types-error #'arg $arg-type
                    (list integer-type string-type))))))))))

  (define (lookup+and $lookup)
    (lookup+ $lookup 'and
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ arg arg* ...)
            (lets
              ($typed-arg (expand-typed $lookup #'arg))
              ($arg-type (typed-type $typed-arg))
              (cond
                ((type=? $arg-type boolean-type)
                  (typed boolean-type
                    `(and
                      ,(typed-value $typed-arg)
                      ,@(map (partial expand-value-of $lookup boolean-type) #'(arg* ...)))))
                ((type=? $arg-type integer-type)
                  (typed integer-type
                    `(($primitive 3 bitwise-and)
                      ,(typed-value $typed-arg)
                      ,@(map (partial expand-value-of $lookup integer-type) #'(arg* ...)))))
                (else
                  (types-error #'arg $arg-type
                    (list boolean-type integer-type))))))))))

  (define (core-lookup)
    (fluent (empty-lookup)
      (lookup+let)
      (lookup+primitive integer+ (function-type (list* integer-type) integer-type) +)
      (lookup+primitive string+ (function-type (list* string-type) string-type) string-append)
      (lookup++)
      (lookup+and)))
)
