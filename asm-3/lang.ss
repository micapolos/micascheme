(library (asm-3 lang)
  (export
    proc data const
    db
    ; dw
    ;op
    ;+
    assembled
    check-assembled)
  (import
    (rename
      (except (asm-3 base) data)
      (+ %+)
      (- %-))
    (asm-3 syntax-block)
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
      (only (asm-3 syntax-block) align)
      (asm-3 org)))

  (define-rule-syntax (op (id x ...) body ...)
    (define-syntax id
      (make-compile-time-value
        (syntax-rules ()
          ((_ x ...) body ...)))))

  (define-rule-syntax (define-dependent id dependent)
    (define-syntax id (make-compile-time-value dependent)))

  (define-syntax (define-fragment $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id x ...)
        #`(define-dependent id
          #,(fragment->syntax
            (block->fragment
              (syntax->block $lookup #'(begin x ...))))))))

  (define-rule-syntax (proc id x ...)
    (define-fragment id x ...))

  (define-rule-syntax (data id x ...)
    (define-fragment id x ...))

  (define-syntax (const $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id x)
        #`(define-dependent id
          #,(expression->syntax
            (syntax->expression $lookup #'x))))))

  ; (define-syntax +
  ;   (make-compile-time-value
  ;     (lambda ($lookup $syntax)
  ;       (syntax-case $syntax ()
  ;         (id
  ;           (identifier? #'id)
  ;           (pure-expression %+))
  ;         ((_ x ...)
  ;           (apply application-expression
  ;             (pure-expression %+)
  ;             (map (partial syntax->expression $lookup) #'(x ...))))))))

  (define-syntax db
    (make-compile-time-value
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x ...)
            (apply db-block
              (map (partial syntax->expression $lookup) #'(x ...))))))))

  (define-syntax dw
    (make-compile-time-value
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x ...)
            (apply dw-block
              (map (partial syntax->expression $lookup) #'(x ...))))))))

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
