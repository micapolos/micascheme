(library (micalog domain)
  (export
    edges+
    event+?
    domain+)
  (import
    (micascheme)
    (micalog utils)
    (syntax scope)
    (prefix (micalog keywords) %))

  (define (timed-domain $timed)
    (syntax-case $timed ()
      ((domain xs ...) #'domain)))

  (define (literal->timed $literal)
    (syntax-case $literal ()
      ((type value)
        #`(%async type value))))

  (define (scope-name->timed $scope $name)
    (syntax-case $name ()
      ((type name)
        #'(
          #,(scope-ref $scope (identifier name))
          type name))))

  ; (define (scope-expr->timed $scope $expr)
  ;   (syntax-case $expr ()
  ;     ((type value)
  ;       (syntax-case #'value ()
  ;         (name (identifier? #'name)
  ;           #`(#,(scope-ref $scope #'name) type name))
  ;         ((%and xs ...)
  ;           (scope-type-op->timed $scope #'type #'value))
  ;         (literal (integer? (datum literal))
  ;           #`(%async type literal))))))

  ; (define (scope-type-op2->timed $scope $type $op2)
  ;   (syntax-case $op2 ()
  ;     ((op a b)
  ;       (lets
  ;         ($timed-a (scope-expr->timed #'a))
  ;         ($timed-b (scope-expr->timed #'b))
  ;         ($domain-a (timed-domain $timed-a))
  ;         ($domain-b (timed-domain $timed-b))
  ;         ($domain (domain+ $domain-a $domain-b))))))

)
