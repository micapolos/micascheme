(library (asm-3 lang)
  (export
    define-asm
    assembled-asm)
  (import
    (micascheme)
    (asm-3 located)
    (asm-3 assembler)
    (asm-3 assembled)
    (asm-3 org))

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
