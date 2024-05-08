(library (minic syntax)
  (export
    parse
    expr? expr expr-type expr-value)
  (import
    (micascheme)
    (minic keyword)
    (prefix (emu math) emu-))

  (data (expr type value))

  (define (parse $syntax)
    (expr-value (syntax->expr $syntax)))

  (define (syntax->expr $syntax)
    (syntax-case $syntax (u8 u8+ u8+1)
      ((u8 value)
        (emu-u8? (datum value))
        (expr #'u8 #'value))
      ((u8+1 rhs)
        (expr #'u8
          #`(emu-u8+1
            #,(syntax->u8-value #'rhs))))
      ((u8+ lhs rhs)
        (expr #'u8
          #`(emu-u8+
            #,(syntax->u8-value #'lhs)
            #,(syntax->u8-value #'rhs))))))

  (define (syntax->u8-value $syntax)
    (expr->u8-value (syntax->expr $syntax)))

  (define (expr->u8-value (expr $type $value))
    (if (and (identifier? $type) (free-identifier=? $type #'u8))
      $value
      (syntax-error $value "not u8")))
)
