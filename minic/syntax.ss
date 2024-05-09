(library (minic syntax)
  (export
    parse
    env? env env-syntax->expr-proc env-syntax-type->value-proc
    expr? expr expr-type expr-value)
  (import
    (micascheme)
    (minic keyword)
    (minic type)
    (minic type-syntax)
    (prefix (emu math) emu-))

  (data (env syntax->expr-proc syntax-type->value-proc))
  (data (expr type value))

  (define (env-syntax->expr $env $syntax)
    (app (env-syntax->expr-proc $env) $syntax))

  (define (env-syntax-type->value $env $syntax $type)
    (app (env-syntax-type->value-proc $env) $syntax $type))

  (define (parse $env $syntax)
    (expr-value (syntax->expr $env $syntax)))

  (define (expr-apply $fn-syntax $fn-expr $arg-syntaxes $arg-exprs)
    (lets
      ($fn-type (expr-type $fn-expr))
      (run (unless (function-type? $fn-type) (syntax-error $fn-syntax "not a function")))
      ($arg-types (map expr-type $arg-exprs))
      ($param-types (function-type-param-types $fn-type))
      ($result-type (function-type-result-type $fn-type))
      ($values (map syntax-expr-type->value $arg-syntaxes $arg-exprs $param-types))
      (expr $result-type #`(#,(expr-value $fn-expr) #,@$values))))

  (define (syntax->expr $env $syntax)
    (or
      (env-syntax->expr $env $syntax)
      (syntax-case $syntax (type u8 u8+ u8+1 u16 u16+1 u16+)
        ((type $x)
          (expr
            (type-type)
            (type->syntax (expr-type (syntax->expr $env #'$x)))))
        ((u8 value)
          (cond
            ((emu-u8? (datum value)) (expr (int-type 8) #'value))
            (else (syntax-error #'value (format "not ~s:" (type->datum (int-type 8)))))))
        ((u16 value)
          (cond
            ((emu-u16? (datum value)) (expr (int-type 16) #'value))
            (else (syntax-error #'value (format "not ~s:" (type->datum (int-type 16)))))))
        (($fn $arg ...)
          (expr-apply
            #'$fn
            (syntax->expr $env #'$fn)
            (syntax->list #'($arg ...))
            (map (partial syntax->expr $env) (syntax->list #'($arg ...)))))
        ($other
          (expr (syntax-type) #'#'$other)))))

  (define (syntax-type->value $env $syntax $type)
    (syntax-expr-type->value $syntax (syntax->expr $env $syntax) $type))

  (define (syntax-expr-type->value $syntax $expr $type)
    (lets
      ($expr-type (expr-type $expr))
      (run
        (unless (equal? $expr-type $type)
          (syntax-error $syntax
            (format "expected type: ~s, actual type: ~s, expression:"
              (type->datum $type)
              (type->datum $expr-type)))))
      (expr-value $expr)))
)
