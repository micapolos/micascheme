(library (tico)
  (export
    expr expr-value expr-type
    empty-context context-push context-ref
    type=?
    parse-expr)
  (import (micascheme))

  (define (expr $value $type)
    #`(#,$value #,$type))

  (define (expr-value $expr)
    (syntax-case $expr ()
      (($value _) #`$value)))

  (define (expr-type $expr)
    (syntax-case $expr ()
      ((_ $type) #`$type)))

  (define (empty-context) #`())

  (define (context-push $context $expr)
    #`(#,$expr #,$context))

  (define (context-ref $context $type)
    (syntax-case $context (cons)
      (() #f)
      (($expr $context)
        (if (type=? (expr-type #`$expr) $type)
          (expr-value #`$expr)
          (context-ref #`$context $type)))))

  (define (type=? $type1 $type2)
    (syntax-case $type1 (lambda)
      ($id1
        (identifier? #`$id1)
        (and
          (identifier? $type2)
          (free-identifier=? #`$id1 $type2)))
      ((lambda ($param1 ...) $body1)
        (syntax-case $type2 (lambda)
          ((lambda ($param2 ...) $body2)
            (and
              (for-all type=?
                (syntax->list #`($param1 ...))
                (syntax->list #`($param2 ...)))
              (type=? #`$body1 #`$body2)))))
      (else #f)))

  (define (parse-expr $context $syntax)
    (syntax-case $syntax (lambda)
      ($other
        (switch (syntax->datum #`$other)
          ((boolean? $boolean) (expr #`$other #`boolean))
          ((number? $number) (expr #`$other #`number))
          ((string? $string) (expr #`$other #`string))
          ((else _)
            (switch (context-ref $context #`$other)
              ((false? _) (syntax-error #`$other "parse-expr"))
              ((else $value) (expr $value #`$other))))))))
)
