(library (simplang expander)
  (export typed expr-of)
  (import (except (micascheme) expand))

  (define (typed $scope $syntax)
    (syntax-case $syntax (if = let : + - length)
      (x (boolean? (datum x)) (cons 'boolean #'x))
      (x (integer? (datum x)) (cons 'integer #'x))
      (x (char? (datum x)) (cons 'char #'x))
      (x (string? (datum x)) (cons 'string #'x))
      (x (symbol? (datum x))
        (switch (assv (datum x) $scope)
          ((pair? (pair _ $type)) (cons $type (datum x)))
          ((else _) (syntax-error #'x "undefined"))))
      (x
        (or
          (typed-syntax? $scope $syntax)
          (typed-application $scope #'x)))))

  (define (typed-syntax? $scope $syntax)
    (syntax-case $syntax ()
      ((x arg ...)
        (symbol? (datum x))
        (switch? (assv (datum x) $scope)
          ((pair? (pair $id $type))
            (switch? $type
              ((pair? (pair $subtype $value))
                (case $subtype
                  ((core) ($value $scope $syntax))
                  ((macro) (typed $scope ($value $scope $syntax)))
                  (else #f)))))))))

  (define (typed-application $scope $syntax)
    (syntax-case $syntax ()
      ((fn arg ...)
        (lets
          ($typed-fn (typed $scope #'fn))
          ($typed-args (map (partial typed $scope) #'(arg ...)))
          (syntax-case (car $typed-fn) (-> core)
            ((-> (param ...) result)
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

  (define (expr-of $scope $type $syntax)
    (lets
      ($typed (typed $scope $syntax))
      (if (equal? (car $typed) $type)
        (cdr $typed)
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s, in" (car $typed) $type)))))
)
