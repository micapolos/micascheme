(library (asm-3 lang)
  (export
    define-asm
    asm)
  (import
    (micascheme)
    (asm-3 located)
    (asm-3 assembler))

  (define-rule-syntax (define-asm id value)
    (define-syntax id (make-compile-time-value value)))

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ org id)
        (and
          (integer? (datum org))
          (identifier? #'id))
        (lets
          ((located $address $bytevector) (assemble $lookup (datum org) #'id))
          #`(located #,$address #,(bytevector->syntax $bytevector))))))
)
