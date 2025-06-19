(library (simplang core)
  (export core-scope)
  (import (micascheme) (simplang expander))

  (define core-scope
    (list
      (macro (lambda $scope $syntax)
        (syntax-case $syntax ()
          ((_ ((type var) ...) body)
            (for-all (dot symbol? syntax->datum) #'(var ...))
            (lets
              ($vars (map syntax->datum #'(var ...)))
              ($types (map syntax->datum #'(type ...)))
              ($scope (append (map cons $vars $types) $scope))
              ($typed-body (typed $scope #'body))
              ($body-type (car $typed-body))
              `(typed
                (arrow (,@$types) ,$body-type)
                (lambda
                  (
                    ,@(filter-opts
                      (map-with
                        ($var $vars)
                        ($type $types)
                        (and (not (macro? $type)) $var))))
                  ,(cdr $typed-body)))))))
      (macro (let $scope $syntax)
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
              `(typed
                ,(car $typed-body)
                (let
                  (
                    ,@(filter-opts
                      (map-with
                        ($var #'(var ...))
                        ($typed $typed-exprs)
                        (and (not (macro? (car $typed))) (list $var (cdr $typed))))))
                  ,(cdr $typed-body)))))))
      (macro (let* $scope $syntax)
        (syntax-case $syntax ()
          ((_ () body) #'body)
          ((_ ((var expr) decl ...) body)
            `(let ((,#'var ,#'expr))
              (let* (,@#'(decl ...))
                ,#'body)))))
      (macro (cond $scope $syntax)
        (syntax-case $syntax (else)
          ((_ (else body)) #'body)
          ((_ (test body) x ...)
            `(if ,#'test ,#'body (cond ,@#'(x ...))))))
      (macro (if $scope $syntax)
        (syntax-case $syntax ()
          ((_ cond true false)
            (lets
              ((pair $type $true) (typed $scope #'true))
              `(typed
                ,$type
                (if
                  ,(expr-of $scope 'boolean #'cond)
                  ,$true
                  ,(expr-of $scope $type #'false)))))))
      (macro (= $scope $syntax)
        (syntax-case $syntax ()
          ((_ a b)
            `(typed
              boolean
              ,(lets
                ((pair $type $a) (typed $scope #'a))
                (case $type
                  ((boolean) `(boolean=? ,$a ,(expr-of $scope $type #'b)))
                  ((integer) `(= ,$a ,(expr-of $scope $type #'b)))
                  ((char) `(char=? ,$a ,(expr-of $scope $type #'b)))
                  ((string) `(string=? ,$a ,(expr-of $scope $type #'b)))))))))
      (macro (+ $scope $syntax)
        (syntax-case $syntax ()
          ((_ arg arg* ...)
            (lets
              ((pair $type $arg) (typed $scope #'arg))
              ($arg* (map (partial expr-of $scope $type) #'(arg* ...)))
              (case $type
                ((integer) `(typed integer (+ ,$arg ,@$arg*)))
                ((string) `(typed string (string-append ,$arg ,@$arg*)))
                (else (syntax-error $syntax
                  (format "invalid argument type ~s, expected integer or string, in" $type))))))))
      (macro (- $scope $syntax)
        (syntax-case $syntax ()
          ((- arg arg* ...)
            `(typed integer
              (-
                ,@(map
                  (partial expr-of $scope 'integer)
                  #'(arg arg* ...)))))))
      (macro (length $scope $syntax)
        (syntax-case $syntax ()
          ((_ arg)
            `(typed integer
              (string-length ,(expr-of $scope 'string #'arg))))))
      (macro (label $scope $syntax)
        (syntax-case $syntax ()
          ((_ id)
            `(typed block-proc
              (lambda ($block)
                (block+labels $block ',(datum id)))))))
      (macro (equ $scope $syntax)
        (syntax-case $syntax ()
          ((_ id expr)
            `(typed block-proc
              (lambda ($block)
                (block+equ $block
                  ',(datum id)
                  ',(datum expr)))))))
      (macro (db $scope $syntax)
        (syntax-case $syntax ()
          ((_ expr)
            `(typed block-proc
              (lambda ($block)
                (block+data $block 1
                  (lambda ($scope)
                    `(lambda ($port)
                      (put-u8 $port
                        ,(expr-of $scope 'integer ',(datum expr)))))))))))
      (macro (dw $scope $syntax)
        (syntax-case $syntax ()
          ((_ expr)
            `(typed block-proc
              (lambda ($block)
                (block+data $block 2
                  (lambda ($scope)
                    `(lambda ($port)
                      (put-u16 $port
                        ,(expr-of $scope 'integer ',(datum expr))
                        (endianness little))))))))))))
)
