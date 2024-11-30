(library (micalog core domain)
  (export
    edges+
    event+?
    domain+)
  (import
    (micascheme)
    (micalog core utils)
    (syntax lookup)
    (prefix (micalog keywords) %))

  (define (timed-domain $timed)
    (syntax-case $timed ()
      ((domain xs ...) #'domain)))

  (define (literal->timed $literal)
    (syntax-case $literal ()
      ((type value)
        #`(%async type value))))

  (define (scope-name->timed $lookup $name)
    (syntax-case $name ()
      ((type name)
        #'(
          #,(lookup-ref $lookup (identifier name))
          type name))))

  ; (define (scope-expr->timed $lookup $expr)
  ;   (syntax-case $expr ()
  ;     ((type value)
  ;       (syntax-case #'value ()
  ;         (name (identifier? #'name)
  ;           #`(#,(lookup-ref $lookup #'name) type name))
  ;         ((%and xs ...)
  ;           (scope-type-op->timed $lookup #'type #'value))
  ;         (literal (integer? (datum literal))
  ;           #`(%async type literal))))))

  ; (define (scope-type-op2->timed $lookup $type $op2)
  ;   (syntax-case $op2 ()
  ;     ((op a b)
  ;       (lets
  ;         ($timed-a (scope-expr->timed #'a))
  ;         ($timed-b (scope-expr->timed #'b))
  ;         ($domain-a (timed-domain $timed-a))
  ;         ($domain-b (timed-domain $timed-b))
  ;         ($domain (domain+ $domain-a $domain-b))))))

)
