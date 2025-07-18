(library (asm-3 fragment)
  (export
    db
    fragment->bytevector)
  (import
    (micascheme)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 expression))

  (data (fragment aligned-sized-binary-expression))

  (define-rule-syntax (expr x)
    (syntax->expression #'x))

  (define-syntax (db $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        #`(fragment
          (aligned 1
            (sized #,(length #'(x ...))
              (combine-expressions
                (lambda ($values) (list->binary (map u8-binary $values)))
                (list (expr x) ...))))))))

  (define (fragment->bytevector $lookup $org $fragment)
    (binary->bytevector
      (expression-ref $lookup $org
        (sized-ref
          (aligned-ref
            (fragment-aligned-sized-binary-expression $fragment))))))
)
