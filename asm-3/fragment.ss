(library (asm-3 fragment)
  (export
    db dw
    fragment->bytevector)
  (import
    (asm-3 base)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm lookable))

  ; fragment -> dependent-aligned-sized-relocable-lookable-binary

  (define-rule-syntax (expr x)
    (syntax->expression #'x))

  (define-syntax (db $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($size (length #'(x ...)))
          #`(dependent-map
            (combine-expressions
              (lambda ($values) (list->binary (map u8-binary $values)))
              (list (expr x) ...))
            (lambda ($relocable-lookable-binary)
              (aligned 1 (sized #,$size $relocable-lookable-binary))))))))

  (define-syntax (dw $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($size (* 2 (length #'(x ...))))
          #`(dependent-map
            (combine-expressions
              (lambda ($values)
                (list->binary
                  (map-with ($value $values)
                    (u16-binary $value (endianness little)))))
              (list (expr x) ...))
            (lambda ($relocable-lookable-binary)
              (aligned 1 (sized #,$size $relocable-lookable-binary))))))))

  (define (fragment->bytevector $org $lookup $fragment)
    (binary->bytevector
      (lookable-ref
        (relocable-ref
          (sized-ref
            (aligned-ref
              (dependent-ref $fragment)))
          $org)
        $lookup)))
)
