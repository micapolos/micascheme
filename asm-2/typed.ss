(library (asm-2 typed)
  (export
    void type boolean integer char string procedure
    typed
    empty-scope
    syntax->typed)
  (import (micascheme) (syntax lookup))

  (define-keywords typed type boolean integer char procedure)

  (define (empty-scope) (empty-lookup))

  (define (syntax->typed $scope $syntax)
    (syntax-case $syntax (void type boolean integer char string procedure lambda + string-append)
      (void #`(typed type void))
      (type #`(typed type type))
      (boolean #`(typed type boolean))
      (integer #`(typed type integer))
      (char #`(typed type char))
      (string #`(typed type string))
      ((procedure (param ...) result)
        #`(typed type
          (procedure
            (#,@(map (partial syntax->expr $scope #'type) #'(param ...)))
            #,(syntax->expr $scope #'type #'result))))
      ((void)
        #`(typed void (void)))
      (b
        (boolean? (datum b))
        #`(typed boolean b))
      (i
        (integer? (datum i))
        #`(typed integer i))
      (ch
        (char? (datum ch))
        #`(typed char ch))
      (str
        (string? (datum str))
        #`(typed string str))
      (+
        #`(typed (procedure (integer integer) integer) +))
      (string-append
        #`(typed (procedure (string string) string) string-append))
      ((lambda ((typ id) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($types (map (partial syntax->expr $scope #'type) #'(typ ...)))
          ($ids #'(id ...))
          ($typed-body
            (syntax->typed
              (fold-left lookup+undefined $scope $ids $types)
              #'body))
          #`(typed
            (procedure (#,@$types) #,(typed-type $typed-body))
            (lambda (#,@$ids) #,(typed-value $typed-body)))))
      ((fn arg ...)
        (syntax-case (syntax->typed $scope #'fn) (typed procedure)
          ((typed (procedure (param ...) result) fn-expr)
            (syntax-case (map (partial syntax->expr $scope) #'(param ...) #'(arg ...)) ()
              ((arg-expr ...)
                #`(typed result (fn-expr arg-expr ...)))))
          ((typed _ _)
            (syntax-error #'fn "not an procedure"))))
      (s
        (symbol? (datum s))
        #`(typed #,(lookup-ref $scope #'s) s))))


  (define (syntax->expr $scope $type $syntax)
    (syntax-case (syntax->typed $scope $syntax) (typed)
      ((typed type expr)
        (cond
          ((type=? #'type $type) #'expr)
          (else
            (syntax-error $syntax
              (format "invalid type ~s, expected ~s, in"
                (syntax->datum #'type)
                (syntax->datum $type))))))))

  (define (type=? $type-a $type-b)
    (equal?
      (syntax->datum $type-a)
      (syntax->datum $type-b)))

  (define (typed-type $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'value)))
)
