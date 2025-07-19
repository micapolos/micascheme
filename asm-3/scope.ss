(library (asm-3 scope)
  (export
    scope scope? scope-identified-values
    empty-scope
    scope-with
    scope+
    scope+identified-value
    scope-ref
    scope->lookup)
  (import
    (micascheme)
    (asm-3 identified))

  (data (scope identified-values))

  (define-rule-syntax (scope-with (id value) ...)
    (fluent (empty-scope)
      (scope+ #'id value) ...))

  (define (empty-scope)
    (scope (list)))

  (define (scope+ $scope $identifier $value)
    (scope
      (cons
        (identified $identifier $value)
        (scope-identified-values $scope))))

  (define (scope+identified-value $scope $identified-value)
    (scope
      (cons
        $identified-value
        (scope-identified-values $scope))))

  (define (scope-ref $scope $identifier)
    (or
      (lets?
        ($found
          (memp
            (lambda ($identified)
              (identified-identifier=? $identified $identifier))
            (scope-identified-values $scope)))
        (identified-ref (car $found)))
      (syntax-error $identifier "undefined")))

  (define (scope->lookup $scope)
    (lambda ($identifier)
      (scope-ref $scope $identifier)))
)
