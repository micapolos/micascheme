(library (asm asm)
  (export
    define-asm
    syntax->asm
    asm-blob
    asm-bytevector
    org)
  (import
    (micascheme)
    (asm block)
    (binary)
    (only (asm typed) syntax->expr))

  (define-keywords org)

  (define-rules-syntax
    ((define-asm id asm)
      (identifier? #'id)
      (define-syntax id (make-compile-time-value asm)))
    ((define-asm (id $lookup $syntax) body)
      (and (identifier? #'$lookup) (identifier? #'$syntax))
      (define-asm id (lambda ($lookup $syntax) body))))

  (meta define (syntax->asm $lookup $syntax)
    (lets
      ($identifier
        (or
          (syntax-selector $syntax)
          (syntax-error $syntax "invalid asm")))
      (app
        (or
          ($lookup $identifier)
          (syntax-error $identifier "undefined asm"))
        $lookup $syntax)))

  (define-syntax (asm-blob $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) asm ...)
        (lets
          ($block
            (fold-left
              (lambda ($block $asm)
                ((syntax->asm $lookup $asm) $block))
              (empty-block)
              #'(asm ...)))
          #`(blob-with ($port #,(block-size $block))
            (put-binary $port
              #,(syntax->expr $lookup #'binary
                (block-binary-syntax
                  (fold-left
                    (lambda ($block $asm)
                      ((syntax->asm $lookup $asm) $block))
                    (empty-block)
                    #'(asm ...))
                  (datum $org)))))))
      ((_ asm ...)
        #`(asm-blob (org 0) asm ...))))

  (define-rule-syntax (asm-bytevector asm ...)
    (blob->bytevector (asm-blob asm ...)))
)
