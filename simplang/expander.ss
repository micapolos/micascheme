(library (simplang expander)
  (export typed expr-of macro?)
  (import (except (micascheme) expand string))

  (define (scope-ref $scope $id)
    (lets
      ($ass? (assv (datum/annotation-stripped $id) $scope))
      (if $ass?
        (cdr $ass?)
        (syntax-error $id "not bound"))))

  (define (typed $scope $syntax)
    (syntax-case $syntax (typed)
      ((typed type expr)
        (cons (datum type) #'expr))
      (x
        (boolean? (datum x))
        (cons 'boolean #'x))
      (x
        (integer? (datum x))
        (cons 'integer #'x))
      (x
        (char? (datum x))
        (cons 'char #'x))
      (x
        (string? (datum x))
        (cons 'string #'x))
      (x
        (symbol? (datum x))
        (cons (scope-ref $scope #'x) (datum x)))
      (x
        (or
          (typed-syntax? $scope $syntax)
          (typed-application $scope #'x)))))

  (define (typed-syntax? $scope $syntax)
    (syntax-case $syntax ()
      ((x arg ...)
        (symbol? (datum x))
        (syntax-case? (scope-ref $scope #'x) (macro)
          ((macro . proc)
            (typed $scope (#'proc $scope $syntax)))))))

  (define (typed-application $scope $syntax)
    (syntax-case $syntax ()
      ((fn arg ...)
        (lets
          ($typed-fn (typed $scope #'fn))
          ($typed-args (map (partial typed $scope) #'(arg ...)))
          (syntax-case (car $typed-fn) (arrow)
            ((arrow (param ...) result)
              (cond
                ((not (= (length #'(param ...)) (length #'(arg ...))))
                  (syntax-error $syntax
                    (format
                      "invalid argument count ~s, expected ~s, in"
                      (length #'(arg ...))
                      (length #'(param ...)))))
                (else
                  (for-each
                    (lambda ($param $arg $type)
                      (unless (equal? $param $type)
                        (syntax-error $arg
                          (format "invalid argument type ~s, expected ~s, in" $type $param))))
                    #'(param ...)
                    #'(arg ...)
                    (map car $typed-args))
                  (cons #'result
                    `(,(cdr $typed-fn) ,@(map cdr $typed-args))))))
            (other
              (syntax-error #'fn
                (format "invalid type ~s, expected procedure, in" #'other))))))))

  (define (macro? $type)
    (syntax-case? $type (macro)
      ((macro . proc) #t)))

  (define (expr-of $scope $type $syntax)
    (lets
      ($typed (typed $scope $syntax))
      (if (equal? (car $typed) $type)
        (cdr $typed)
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s, in" (car $typed) $type)))))
)
