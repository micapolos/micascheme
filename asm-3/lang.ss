(library (asm-3 lang)
  (export
    proc data const
    db dw
    op
    +
    assembled
    check-assembled)
  (import
    (rename
      (except (asm-3 base) data)
      (+ %+)
      (- %-))
    (asm-3 syntax-block)
    (asm-3 expression-syntax)
    (asm-3 block-fragment)
    (asm-3 assembler)
    (asm-3 expression)
    (asm-3 block)
    (asm-3 syntax-expression)
    (asm-3 fragment)
    (except (asm-3 assembled) assembled)
    (asm-3 org))
  (export
    (import
      (only (asm-3 base) syntax begin)
      (only (asm-3 block) block)
      (only (asm-3 syntax-block) label)
      (asm-3 org)
      (asm-3 expression-syntax)))

  (define-rule-syntax (op (id x ...) body ...)
    (define-syntax id
      (make-compile-time-value
        (syntax-rules ()
          ((_ x ...) body ...)))))

  (define-rule-syntax (define-asm id value)
    (define-syntax id (make-compile-time-value value)))

  (define-rule-syntax (proc id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (data id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (const id x)
    (define-asm id (expr x)))

  (define-syntax +
    (make-compile-time-value
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (pure-expression %+))
          ((_ x ...)
            (apply application-expression
              (pure-expression %+)
              (map (partial syntax->expression $lookup) #'(x ...))))))))

  (define-syntax db
    (make-compile-time-value
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x ...)
            (list->block
              (map
                (lambda ($syntax)
                  (u8-expression-block (syntax->expression $lookup $syntax)))
                #'(x ...))))))))

  (define-syntax dw
    (make-compile-time-value
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x ...)
            (list->block
              (map
                (lambda ($syntax)
                  (u16-expression-block
                    (syntax->expression $lookup $syntax)
                    (endianness little)))
                #'(x ...))))))))

  (define-syntax (assembled-proc $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) id)
        (and
          (integer? (datum $org))
          (identifier? #'id))
        (assembled->syntax
          (assemble-identifier $lookup (datum $org) #'id)))))

  (define-syntax (assembled $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) x ...)
        (integer? (datum $org))
        (assembled->syntax
          (assemble-fragment $lookup (datum $org)
            (block->fragment
              (syntax->block $lookup #'(begin x ...))))))))

  (define-rule-syntax (check-assembled (org $org) x ... out)
    (check (equal? (assembled->datum (assembled (org $org) x ...)) 'out)))
)
