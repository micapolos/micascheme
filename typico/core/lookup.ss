(library (typico core lookup)
  (export core-lookup)
  (import (micascheme) (typico typed) (typico expand) (typico type) (typico core types) (typico lookup) (asm u))

  (define-rule-syntax (lookup+primitive-type $lookup id type)
    (lookup+ $lookup 'id
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (symbol? (datum id))
            (typed type-type 'type))
          (other
            (expand-typed/no-lookup $lookup #'other))))))

  (define (lookup+function $lookup)
    (lookup+ $lookup 'function
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ (param ... last-param tail) result)
            (equal? (datum tail) '...)
            (typed type-type
              `(function-type
                (list*
                  ,@(map (partial expand-value-of $lookup type-type) #'(param ...))
                  ,(expand-value-of $lookup type-type #'last-param))
                ,(expand-value-of $lookup type-type #'result))))
          ((_ (param ...) result)
            (typed type-type
              `(function-type
                (list ,@(map (partial expand-value-of $lookup type-type) #'(param ...)))
                ,(expand-value-of $lookup type-type #'result))))))))

  (define (lookup+typeof $lookup)
    (lookup+ $lookup 'typeof
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x)
            (expand-typed $lookup
              (type->datum
                (typed-type
                  (expand-typed $lookup #'x)))))))))

  (define (lookup+bytevector $lookup)
    (lookup+ $lookup 'bytevector
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ u8 ...)
            (typed bytevector-type
              `(($primitive 3 bytevector)
                ,@(map
                  (partial expand-predicate-value-of $lookup u8? u8-type)
                  #'(u8 ...)))))
          (id
            (symbol? (datum id))
            (typed type-type 'bytevector-type))))))

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
            (symbol? (datum id))
            (typed type `($primitive 3 out)))
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

  (define (lookup+length $lookup)
    (lookup+ $lookup 'length
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x)
            (lets
              ($type (typed-type (expand-typed $lookup #'x)))
              (cond
                ((type=? $type string-type) (expand-typed $lookup #'(string-length x)))
                ((type=? $type bytevector-type) (expand-typed $lookup #'(bytevector-length x)))
                (else (types-error #'x $type (list string-type bytevector-type))))))))))

  (define (lookup+string $lookup)
    (lookup+ $lookup 'string
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (symbol? (datum id))
            (typed type-type 'string-type))
          ((_ x)
            (typed string-type
              `(format "~s" ,(typed-value (expand-typed $lookup #'x)))))))))

  (define (core-lookup)
    (fluent (empty-lookup)
      (lookup+primitive-type boolean boolean-type)
      (lookup+primitive-type integer integer-type)
      (lookup+primitive-type char char-type)

      (lookup+primitive-type u2 u2-type)
      (lookup+primitive-type u3 u3-type)
      (lookup+primitive-type u7 u7-type)
      (lookup+primitive-type u8 u8-type)
      (lookup+primitive-type u16 u16-type)
      (lookup+primitive-type s8 s8-type)

      (lookup+bytevector)
      (lookup+function)
      (lookup+typeof)
      (lookup+length)
      (lookup+string)

      (lookup+let)

      (lookup+primitive boolean=? (function-type (list boolean-type boolean-type) boolean-type) boolean=?)
      (lookup+primitive integer=? (function-type (list integer-type integer-type) boolean-type) =)
      (lookup+primitive char=? (function-type (list char-type char-type) boolean-type) char=?)
      (lookup+primitive string=? (function-type (list string-type string-type) boolean-type) string=?)
      (lookup+primitive bytevector=? (function-type (list bytevector-type bytevector-type) boolean-type) bytevector=?)

      (lookup+primitive integer+ (function-type (list* integer-type) integer-type) +)
      (lookup+primitive string+ (function-type (list* string-type) string-type) string-append)

      (lookup+primitive string-length (function-type (list string-type) integer-type) string-length)
      (lookup+primitive bytevector-length (function-type (list bytevector-type) integer-type) bytevector-length)
      (lookup+primitive integer->string (function-type (list integer-type) string-type) number->string)

      (lookup+=)
      (lookup++)
      (lookup+and)))
)
