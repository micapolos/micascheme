(library (simplang expander)
  (export typed)
  (import (except (micascheme) expand))

  (define (typed $scope $syntax)
    (syntax-case $syntax (let : + - length)
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
      ((+ arg arg* ...)
        (lets
          ((pair $type $arg) (typed $scope #'arg))
          ($arg* (map (partial expr-of $scope $type) #'(arg* ...)))
          (case $type
            ((integer) `(integer . (+ ,$arg ,@$arg*)))
            ((string) `(string . (string-append ,$arg ,@$arg*)))
            (else (syntax-error $syntax
              (format "invalid argument type ~s, expected integer or string, in" $type))))))
      ((- arg arg* ...)
        `(integer . (- ,@(map (partial expr-of $scope 'integer) #'(arg arg* ...)))))
      ((length arg)
        `(integer . (string-length ,(expr-of $scope 'string #'arg))))
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

  (define (expr-of $scope $type $syntax)
    (lets
      ($typed (typed $scope $syntax))
      (if (equal? (car $typed) $type)
        (cdr $typed)
        (syntax-error $syntax
          (format "invalid type ~s, expected ~s, in" (car $typed) $type)))))
)
