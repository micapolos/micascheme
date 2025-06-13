(library (simplang expander)
  (export typed std-scope)
  (import (except (micascheme) expand))

  (define std-scope
    '(
      (+ . (-> (integer integer) integer))
      (string-append . (-> (string string) string))
      (string-length . (-> (string) integer))))

  (define (typed $scope $syntax)
    (syntax-case $syntax (let :)
      (x (boolean? #'x) (cons 'boolean #'x))
      (x (integer? #'x) (cons 'integer #'x))
      (x (char? #'x) (cons 'char #'x))
      (x (string? #'x) (cons 'string #'x))
      ((: type x) (symbol? #'x) (cons #'type #'x))
      (x (symbol? #'x)
        (switch (assv #'x $scope)
          ((pair? (pair $symbol $type)) (cons $type #'x))
          ((else _) (syntax-error $syntax "undefined"))))
      ((let ((var expr) ...) body)
        (lets
          ($typed-exprs (map (partial typed $scope) #'(expr ...)))
          ($typed-body (typed (append (map cons #'(var ...) (map car $typed-exprs)) $scope) #'body))
          (cons (car $typed-body)
            `(let (,@(map list #'(var ...) (map cdr $typed-exprs)))
              ,(cdr $typed-body)))))
      ((fn arg ...)
        (lets
          ($typed-fn (typed $scope #'fn))
          ($typed-args (map (partial typed $scope) #'(arg ...)))
          (syntax-case (car $typed-fn) (->)
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
)
