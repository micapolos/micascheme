(library (minic syntax)
  (export
    parse
    expr? expr expr-type expr-value)
  (import
    (micascheme)
    (minic keyword)
    (minic type)
    (prefix (emu math) emu-))

  (data (env syntax->expr-proc))
  (data (expr type value))

  (define (env-syntax->expr $env $syntax)
    (app (env-syntax->expr-proc $env) $syntax))

  (define (parse $syntax)
    (expr-value
      (syntax->expr
        (env (lambda (_) #f))
        $syntax)))

  (define (syntax->expr $env $syntax)
    (or
      (env-syntax->expr $env $syntax)
      (syntax-case $syntax (type u8 u8+ u8+1 u16 u16+1 u16+)
        ((type $x)
          (expr
            (type-type)
            (type->syntax (expr-type (syntax->expr $env #'$x)))))
        (u8+
          (expr
            (function-type (list (int-type 8) (int-type 8)) (int-type 8))
            #'(lambda (x y) (u8+ x y))))
        ((u8 value)
          (cond
            ((emu-u8? (datum value)) (expr (int-type 8) #'value))
            (else (syntax-error #'value (format "not ~s:" (type->datum (int-type 8)))))))
        ((u8+1 rhs)
          (expr (int-type 8)
            #`(emu-u8+1
              #,(syntax-type->value $env #'rhs (int-type 8)))))
        ((u8+ lhs rhs)
          (expr (int-type 8)
            #`(emu-u8+
              #,(syntax-type->value $env #'lhs (int-type 8))
              #,(syntax-type->value $env #'rhs (int-type 8)))))
        ((u16 value)
          (cond
            ((emu-u16? (datum value)) (expr (int-type 16) #'value))
            (else (syntax-error #'value (format "not ~s:" (type->datum (int-type 16)))))))
        ((u16+1 rhs)
          (expr (int-type 16)
            #`(emu-u16+1
              #,(syntax-type->value $env #'rhs (int-type 16)))))
        ((u16+ lhs rhs)
          (expr (int-type 16)
            #`(emu-u16+
              #,(syntax-type->value $env #'lhs (int-type 16))
              #,(syntax-type->value $env #'rhs (int-type 16))))))))

  (define (syntax-type->value $env $syntax $type)
    (lets
      ($expr (syntax->expr $env $syntax))
      ($expr-type (expr-type $expr))
      (run
        (unless (equal? $expr-type $type)
          (syntax-error $syntax
            (format "expected ~s, actual ~s:"
              (type->datum $type)
              (type->datum $expr-type)))))
      (expr-value $expr)))
)
