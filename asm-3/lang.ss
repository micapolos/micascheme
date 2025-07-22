(library (asm-3 lang)
  (export
    proc data const native
    assembled-asm)
  (import
    (except (asm-3 base) data begin)
    (asm-3 block-syntax)
    (asm-3 expression-syntax)
    (asm-3 block-fragment)
    (asm-3 assembler)
    (asm-3 assembled))
  (export
    (import
      (asm-3 org)
      (asm-3 expression-syntax)
      (asm-3 block-syntax)))

  (define-rule-syntax (proc id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (data id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (const id x)
    (define-asm id (expr x)))

  (define-rule-syntax (native id x)
    (define-expr id x))

  (define-rule-syntax (define-asm id value)
    (define-syntax id (make-compile-time-value value)))

  (define-syntax (assembled-asm $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) id)
        (and
          (integer? (datum $org))
          (identifier? #'id))
        (assembled->syntax (assemble $lookup (datum $org) #'id)))))
)
