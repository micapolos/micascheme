(library (typed evaluate)
  (export
    scope scope-ref
    hole hole?
    thunk thunk? thunk-max-index thunk-datum-proc
    evaluate-syntax)
  (import
    (micascheme)
    (evaluator)
    (any)
    (typed lang)
    (typed typed)
    (typed type))

  (data (thunk max-index datum-proc))
  (data hole)

  (define-rule-syntax (scope (symbol value) ...)
    (stack (cons 'symbol value) ...))

  (define (datum->value $datum)
    (eval $datum (environment '(micascheme))))

  (define (scope+ $scope $symbol $value)
    (push $scope (cons $symbol $value)))

  (define (scope-ref $scope $symbol)
    (indexed-find
      (lambda ($index $ass)
        (and
          (symbol=? (car $ass) $symbol)
          (cons $index (cdr $ass))))
      $scope))

  (define (evaluate-identifier $scope $identifier)
    (lets
      ($symbol (syntax->datum $identifier))
      ((pair $index $typed)
        (or
          (scope-ref $scope $symbol)
          (syntax-error $identifier "undefined")))
      ($type (typed-type $typed))
      (typed $type
        (switch (typed-value $typed)
          ((hole? _)
            (thunk $index
              (lambda () $symbol)))
          ((else $other)
            $other)))))

  (define (evaluate-value $scope $type $syntax)
    (lets
      ($typed (evaluate-syntax $scope $syntax))
      (if (type=? (typed-type $typed) $type)
        (typed-value $typed)
        (syntax-error $syntax "invalid type"))))

  (define (evaluate-type $scope $syntax)
    (switch (evaluate-value $scope any-type $syntax)
      ((thunk? _)
        (syntax-error $syntax "type not constant"))
      ((else $type)
        $type)))

  (define (evaluate-syntax $scope $syntax)
    (syntax-case $syntax (assume assume-type any-type any-string any-lambda lambda)
      ((assume type value)
        (typed
          (evaluate-type $scope #'type)
          (datum->value (datum value))))
      ((assume-type value)
        (typed
          any-type
          (datum->value (datum value))))
      (any-type
        (typed any-type any-type))
      (any-string
        (typed any-type any-string))
      ((any-lambda (param ...) result)
        (typed any-type
          (make-any-lambda
            (map (partial evaluate-type $scope) (syntaxes param ...))
            (evaluate-type $scope #'result))))
      (x
        (string? (datum x))
        (typed any-string (datum x)))
      ((lambda (param ...) body)
        (lets
          ($params (syntaxes param ...))
          ($params-length (length $params))
          ($typed-params
            (map
              (partial evaluate-param $scope)
              $params))
          ($scope
            (fold-left
              (lambda ($scope $typed-param)
                (scope+ $scope
                  (typed-value $typed-param)
                  (typed (typed-type $typed-param) hole)))
              $scope
              $typed-params))
          ($typed-body
            (evaluate-syntax $scope #'body))
          (typed
            (make-any-lambda
              (map typed-type $typed-params)
              (typed-type $typed-body))
            (switch (typed-value $typed-body)
              ((thunk? (thunk $max-index $datum-proc))
                (lets
                  ($datum
                    `(lambda (,(map typed-value $typed-params))
                      ,($datum-proc)))
                  (cond
                    ((< $max-index $params-length)
                      (datum->value $datum))
                    (else
                      (thunk
                        (- $max-index $params-length)
                        `(lambda () ,$datum))))))
              ((else $value)
                (lets
                  ($tmp (gensym))
                  (
                    (datum->value
                      `(lambda (,$tmp)
                        (lambda (,@(map typed-value $typed-params))
                          ,$tmp)))
                    $value)))))))
      (x
        (symbol? (datum x))
        (evaluate-identifier $scope #'x))))

  (define (syntax->symbol $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (if (symbol? $datum)
        $datum
        (syntax-error $syntax "invalid identifier"))))

  (define (evaluate-param $scope $param)
    (syntax-case $param ()
      ((type id)
        (lets
          ($symbol (syntax->symbol #'id))
          ($type (evaluate-type $scope #'type))
          (scope+ $scope $symbol (typed $type $symbol))))
      (other
        (syntax-error #'other "invalid param"))))
)
