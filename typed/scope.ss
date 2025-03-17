(library (typed scope)
  (export
    scope scope+ scope-ref)
  (import
    (micascheme))

  (define-rule-syntax (scope (symbol value) ...)
    (stack (cons 'symbol value) ...))

  (define (scope+ $scope $symbol $value)
    (push $scope (cons $symbol $value)))

  (define (scope-ref $scope $symbol)
    (indexed-find
      (lambda ($index $ass)
        (and
          (symbol=? (car $ass) $symbol)
          (cons $index (cdr $ass))))
      $scope))
)
