(library (asm-3 lang)
  (export
    proc const
    define-asm
    assembled-asm)
  (import
    (asm-3 base)
    (asm-3 located)
    (asm-3 syntax-fragment)
    (asm-3 syntax-expression)
    (asm-3 assembler)
    (asm-3 assembled)
    (asm-3 org))
  (export
    (import
      (only (asm-3 base) begin)
      (only (asm-3 org) org)
      (only (asm-3 syntax-block) db dw align)))

  (define-rule-syntax (proc id x ...)
    (define-asm id (syntax->fragment #'(begin x ...))))

  (define-rule-syntax (const id x)
    (define-asm id (syntax->expression #'x)))

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
