(library (simplang core)
  (export core-scope)
  (import (micascheme) (simplang expander))

  (define core-scope
    (list
      (cons 'let
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((_ ((var expr) ...) body)
                (for-all (dot symbol? syntax->datum) #'(var ...))
                (lets
                  ($typed-exprs (map (partial typed $scope) #'(expr ...)))
                  ($typed-body
                    (typed
                      (append
                        (map cons
                          (map syntax->datum #'(var ...))
                          (map car $typed-exprs))
                        $scope)
                      #'body))
                  `(:
                    ,(car $typed-body)
                    (let (,@(map list #'(var ...) (map cdr $typed-exprs)))
                      ,(cdr $typed-body)))))))))
      (cons 'cond
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax (else)
              ((_ (else body)) #'body)
              ((_ (test body) x ...)
                #'(if test body (cond x ...)))))))
      (cons 'if
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((_ cond true false)
                (lets
                  ((pair $type $true) (typed $scope #'true))
                  `(:
                    ,$type
                    (if
                      ,(expr-of $scope 'boolean #'cond)
                      ,$true
                      ,(expr-of $scope $type #'false)))))))))
      (cons '=
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((_ a b)
                `(:
                  boolean
                  ,(lets
                    ((pair $type $a) (typed $scope #'a))
                    (case $type
                      ((boolean) `(boolean=? ,$a ,(expr-of $scope $type #'b)))
                      ((integer) `(= ,$a ,(expr-of $scope $type #'b)))
                      ((char) `(char=? ,$a ,(expr-of $scope $type #'b)))
                      ((string) `(string=? ,$a ,(expr-of $scope $type #'b)))))))))))
      (cons '+
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((_ arg arg* ...)
                (lets
                  ((pair $type $arg) (typed $scope #'arg))
                  ($arg* (map (partial expr-of $scope $type) #'(arg* ...)))
                  (case $type
                    ((integer) `(: integer (+ ,$arg ,@$arg*)))
                    ((string) `(: string (string-append ,$arg ,@$arg*)))
                    (else (syntax-error $syntax
                      (format "invalid argument type ~s, expected integer or string, in" $type))))))))))
      (cons '-
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((- arg arg* ...)
                `(: integer
                  (-
                    ,@(map
                      (partial expr-of $scope 'integer)
                      #'(arg arg* ...)))))))))
      (cons 'length
        (cons 'macro
          (lambda ($scope $syntax)
            (syntax-case $syntax ()
              ((_ arg)
                `(: integer (string-length ,(expr-of $scope 'string #'arg))))))))))
)
