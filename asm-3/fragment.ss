(library (asm-3 fragment)
  (export
    db
    fragment->bytevector)
  (import
    (micascheme)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 expression)
    (asm-3 dependent))

  (data (fragment aligned-sized-binary-expression))

  (define-rule-syntax (expr x)
    (syntax->dependent-expression #'x))

  (define-syntax (db $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        #`(dependent-map
          (lambda ($expression)
            (fragment
              (aligned 1
                (sized #,(length #'(x ...))
                  $expression))))
          (combine-dependent-expressions
            (lambda ($values) (list->binary (map u8-binary $values)))
            (list (expr x) ...))))))

  (define (fragment->bytevector $lookup $org $fragment)
    (binary->bytevector
      (expression-ref $lookup $org
        (sized-ref
          (aligned-ref
            (fragment-aligned-sized-binary-expression $fragment))))))
)
