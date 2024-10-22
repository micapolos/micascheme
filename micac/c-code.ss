(library (micac c-code)
  (export
    type->c-code
    term->c-code
    expr->c-code
    statement->c-code)
  (import (micascheme) (micac ast) (code))

  (data (c-body code size))

  (define (c-body+code $c-body $code)
    (c-body-with-code $c-body
      (code (c-body-code $c-body) $code)))

  (define (block-code $code)
    (code-in-curly-brackets
      (code-indent
        (code "\n" $code))))

  (define (type->c-code $type)
    (type-switch $type
      ((bool? _) (code "bool"))
      ((u8? _) (code "uint8_t"))
      ((u16? _) (code "uint16_t"))
      ((u32? _) (code "uint32_t"))
      ((struct? $struct)
        (space-separated-code
          "struct"
          (block-code
            (c-body-code
              (types->c-body (struct-types $struct))))))))

  (define (types->c-body $types)
    (fold-left
      c-body+type
      (c-body (code) 0)
      (reverse $types)))

  (define (c-body+type $c-body $type)
    (c-body
      (code
        (c-body-code $c-body)
        (space-separated-code
          (type->c-code $type)
          (string-code (string-append "_" (number->string (c-body-size $c-body)))))
        ";\n")
      (+ (c-body-size $c-body) 1)))

  (define (c-body+statement $c-body $statement)
    (statement-switch $statement
      ((block? $block)
        (c-body+block $c-body $block))
      ((return? $return)
        (c-body+code $c-body
          (code
            (space-separated-code "return"
              (expr->c-code (return-expr $return)))
            ";")))))

  (define (term->c-code $term)
    (term-switch $term
      ((const? $const) (const->c-code $const))))

  (define (const->c-code $const)
    (string-code (number->string (const-value $const))))

  (define (expr->c-code $expr)
    (term->c-code (expr-term $expr)))

  (define (c-body+block $c-body $block)
    (lets
      ($types-c-body
        (fold-left
          c-body+type
          (c-body (code) (c-body-size $c-body))
          (reverse (block-types $block))))
      ($statements-c-body
        (fold-left
          c-body+statement
          (c-body (code) (c-body-size $types-c-body))
          (reverse (block-statements $block))))
      (c-body
        (block-code
          (code
            (c-body-code $types-c-body)
            (c-body-code $statements-c-body)))
        (c-body-size $statements-c-body))))

  (define (statement->c-code $statement)
    (c-body-code
      (c-body+statement
        (c-body (code) 0)
        $statement)))
)
