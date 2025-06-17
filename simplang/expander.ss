(library (simplang expander)
  (export
    typed expr-of macro?
    type boolean integer char arrow macro)
  (import (except (micascheme) expand string))

  (define-keywords type boolean integer char arrow macro)

  (define (scope-ref $scope $id)
    (lets
      ($ass? (assv (datum/annotation-stripped $id) $scope))
      (if $ass?
        (cdr $ass?)
        (syntax-error $id "not bound"))))

  (define (typed $scope $syntax)
    (syntax-case $syntax (typed type boolean integer char string arrow macro)
      ((typed typ expr) (cons (datum typ) #'expr))
      (type (cons 'type #f))
      (boolean (cons 'type #f))
      (integer (cons 'type #f))
      (char (cons 'type #f))
      (string (cons 'type #f))
      ((arrow (param ...) result) (cons 'type #f))
      (macro (cons 'type #f))
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
                  (cons #'result
                    `(
                      ,(cdr $typed-fn)
                      ,@map (partial expr-of $scope) #'(param ...) #'(arg ...))))))
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
