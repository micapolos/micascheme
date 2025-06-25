(library (typico core lookup)
  (export core-lookup)
  (import (micascheme) (typico typed) (typico expand) (typico type) (typico core types) (typico lookup))

  (define-rule-syntax (lookup+primitive-type $lookup id type)
    (lookup+ $lookup 'id
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (typed type-type type))
          (other
            (expand-typed/no-lookup $lookup #'other))))))

  (define (lookup+typeof $lookup)
    (lookup+ $lookup 'typeof
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x)
            (typed type-type
              (typed-type (expand-typed $lookup #'x))))))))

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

  (define (lookup+= $lookup)
    (lookup+ $lookup '=
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ a b)
            (lets
              ($type (typed-type (expand-typed $lookup #'a)))
              (cond
                ((type=? $type boolean-type) (expand-typed $lookup #'(boolean=? a b)))
                ((type=? $type integer-type) (expand-typed $lookup #'(integer=? a b)))
                ((type=? $type char-type) (expand-typed $lookup #'(char=? a b)))
                ((type=? $type string-type) (expand-typed $lookup #'(string=? a b)))
                ((type=? $type bytevector-type) (expand-typed $lookup #'(bytevector=? a b)))
                (else
                  (types-error #'a $type
                    (list boolean-type integer-type char-type string-type bytevector-type))))))))))

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
      (lookup+primitive-type boolean boolean-type)
      (lookup+primitive-type integer integer-type)
      (lookup+primitive-type char char-type)
      (lookup+primitive-type string string-type)
      (lookup+primitive-type bytevector bytevector-type)
      (lookup+typeof)

      (lookup+let)

      (lookup+primitive boolean=? (function-type (list boolean-type boolean-type) boolean-type) boolean=?)
      (lookup+primitive integer=? (function-type (list integer-type integer-type) boolean-type) =)
      (lookup+primitive char=? (function-type (list char-type char-type) boolean-type) char=?)
      (lookup+primitive string=? (function-type (list string-type string-type) boolean-type) string=?)
      (lookup+primitive bytevector=? (function-type (list bytevector-type bytevector-type) boolean-type) bytevector=?)

      (lookup+primitive integer+ (function-type (list* integer-type) integer-type) +)
      (lookup+primitive string+ (function-type (list* string-type) string-type) string-append)

      (lookup+=)
      (lookup++)
      (lookup+and)))
)
