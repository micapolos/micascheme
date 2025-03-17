(library (typed evaluate)
  (export
    scope scope-ref
    hole hole?
    thunk thunk? thunk-depth thunk-datum-proc
    evaluate-syntax)
  (import
    (micascheme)
    (evaluator)
    (any)
    (typed lang)
    (typed typed)
    (typed type))

  (data (thunk depth datum-proc))
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
            (thunk
              (+ $index 1)
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
    (syntax-case $syntax (assume assume-type lambda)
      ((assume type value)
        (typed
          (evaluate-type $scope #'type)
          (datum->value (datum value))))
      ((assume-type value)
        (typed
          any-type
          (datum->value (datum value))))
      (x
        (symbol? (datum x))
        (evaluate-identifier $scope #'x))
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
              ((thunk? (thunk $depth $datum-proc))
                (lets
                  ($datum
                    `(lambda (,(map typed-value $typed-params))
                      ,($datum-proc)))
                  (cond
                    ((<= $depth $params-length)
                      (datum->value $datum))
                    (else
                      (thunk
                        (- $depth $params-length)
                        `(lambda () ,$datum))))))
              ((else $value)
                (lets
                  ($tmp (gensym))
                  (thunk $params-length
                    (lambda ()
                      (
                        (datum->value
                          `(lambda (,$tmp)
                            (lambda (,(map typed-value $typed-params))
                              ,$tmp)))
                        $value)))))))))))

  (define (evaluate-param $scope $param)
    (syntax-case $param ()
      ((type id)
        (lets
          ($symbol (identifier id))
          ($type (evaluate-type $scope #'type))
          (scope+ $scope $symbol (typed $type $symbol))))))
)
