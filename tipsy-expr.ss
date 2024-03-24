(library (tipsy-expr)
  (export
    expr expr? expr-type expr-value

    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?
    lambda-type lambda-type? lambda-type-param-types lambda-type-result-type
    type-type type-type?
    list-type list-type? list-type-item-type

    Boolean
    Number
    String
    Lambda
    Type
    List

    expr-syntax
    expr-self-syntax

    syntax-expr
    typeof

    of)
  (import
    (micascheme)
    (check))

  (data (expr type value))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (lambda-type param-types result-type))
  (data (list-type item-type))
  (data (type-type))

  (define-syntax Boolean
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ #`(boolean-type)))))

  (define-syntax Number
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ #`(number-type)))))

  (define-syntax String
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ #`(string-type)))))

  (define-syntax Lambda
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($param ...) $result)
          #`(lambda-type (list $param ...) $result)))))

  (define-syntax Type
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ #`(type-type)))))

  (define-syntax List
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item) #`(list-type $item)))))

  (enum
    (type
      number-type
      string-type
      lambda-type
      type-type))

  (define-aux-keyword of)

  (define (expr-apply $lhs $args)
    (expr
      #`(lets
        ($target-var #,(expr-type $lhs))
        ($args-var (list #,@(map expr-type $args)))
        (run
          (unless (lambda-type? $target-var)
            (syntax-error #'#,(expr-value $lhs) "not lambda-type"))
          (unless (for-all equal? (lambda-type-param-types $target-var) $args-var)
            (syntax-error #'(#,@(map expr-value $args)) "invalid args"))
          (lambda-type-result-type $target-var)))
      #`(
        #,(expr-value $lhs)
        #,@(map expr-value $args))))

  (define (expr-if $cond $true $false)
    (expr
      #`(run
        (unless (equal? #,(expr-type $cond) (boolean-type))
          (syntax-error #'#,(expr-value $cond) "must be boolean"))
        (unless (equal? #,(expr-type $true) #,(expr-type $false))
          (syntax-error #'#,(expr-value $false) "must be same type as the other"))
        #,(expr-type $true))
      #`(if
        #,(expr-value $cond)
        #,(expr-value $true)
        #,(expr-value $false))))

  (define (expr-syntax $expr)
    #`(lets
      (_ #,(expr-type $expr))
      #,(expr-value $expr)))

  (define (expr-self-syntax $expr)
    #`(expr
      #,(expr-type $expr)
      #,(expr-value $expr)))

  (define-aux-keyword typeof)

  (define (syntax-expr $syntax $lookup)
    (syntax-case $syntax (define if lambda typeof of)
      ((of $value $type)
        (expr #'$type #'$value))
      ($number
        (number? (datum $number))
        (expr #'(number-type) #'$number))
      ($boolean
        (boolean? (datum $boolean))
        (expr #'(boolean-type) #'$boolean))
      ($string
        (string? (datum $string))
        (expr #'(string-type) #'$string))
      ($symbol
        (symbol? (datum $symbol))
        (or
          ($lookup #'$symbol)
          (syntax-error #'$symbol "untyped")))
      ((if $cond $true $false)
        (expr-if
          (syntax-expr $lookup #'$cond)
          (syntax-expr $lookup #'$true)
          (syntax-expr $lookup #'$false)))
      ((lambda (($type $id) ...) $body)
        (lets
          ($locals
            (map cons
              (syntax->list #'($id ...))
              (syntax->list #'($type ...))))
          ($lookup
            (lambda ($id)
              (lets
                ($ass (assp (partial bound-identifier=? $id) $locals))
                (if $ass (expr (cdr $ass) (car $ass)) ($lookup $id)))))
          ($body-expr (syntax-expr $lookup #'$body))
          (expr
            #`(lambda-type (list $type ...)
              #,(expr-type $body-expr))
            #`(lambda ($id ...)
              #,(expr-value $body-expr)))))
      ((typeof $expr)
        (expr
          #'(type-type)
          (expr-type (syntax-expr $lookup #'$expr))))
      (($lhs $rhs ...)
        (expr-apply
          (syntax-expr $lookup #'$lhs)
          (map
            (partial syntax-expr $lookup)
            (syntax->list #'($rhs ...)))))))
)
