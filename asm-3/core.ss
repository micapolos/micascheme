(library (asm-3 core)
  (export
    db
    fragment->datum
    check-fragment)
  (import
    (micascheme)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-2 relocable))

  (define (syntax->expression $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        (dependent (list #'id)
          (lookable ($lookup)
            (relocable-with ($org)
              ($lookup #'id)))))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (dependent (list)
          (lookable ($lookup)
            (relocable-with ($org)
              (datum literal)))))
      (other
        (syntax-error #'other "invalid expression"))))

  (define (combine-expressions $proc $expressions)
    (dependent-map
      (lambda ($lookables)
        (lookable-map
          (lambda ($relocables)
            (relocable-map $proc
              (list->relocable $relocables)))
          (list->lookable $lookables)))
      (list->dependent $expressions)))

  (define (list->fragment $fragments)
    (aligned-map
      (lambda ($sized-list)
        (sized-map
          (lambda ($binary-expressions)
            (combine-expressions list->binary $binary-expressions)))
        (list->sized $sized-list))
      (list->aligned $fragments)))

  (define-syntax (db $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        #`(aligned 1
          (sized #,(length #'(x ...))
            (combine-expressions
              (lambda ($values) (list->binary (map u8-binary $values)))
              (map syntax->expression #'(x ...))))))))

  (define-syntax (block $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        #`(list->fragment (list x ...)))))
)
