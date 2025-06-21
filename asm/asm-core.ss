(library (asm asm-core)
  (export label db dw block local)
  (import
    (micascheme)
    (asm asm)
    (only (asm block) empty-block block+binary-syntax-proc block+label block+local)
    (only (asm binary) db-binary dw-binary)
    (only (binary) binary-append))

  (define-asm (db $lookup $syntax)
    (syntax-case $syntax ()
      ((_ expr ...)
        (lambda ($block)
          (block+binary-syntax-proc $block 1
            (lambda ($org) #'(binary-append (db-binary expr) ...)))))))

  (define-asm (dw $lookup $syntax)
    (syntax-case $syntax ()
      ((_ expr ...)
        (lambda ($block)
          (block+binary-syntax-proc $block 2
            (lambda ($org) #'(binary-append (dw-binary expr) ...)))))))

  (define-asm (label $lookup $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (lambda ($block)
          (block+label $block #'id)))))

  (define-asm (block $lookup $syntax)
    (syntax-case $syntax ()
      ((_ asm ...)
        (lets
          ($asm (syntaxes->asm $lookup #'(asm ...)))
          (lambda ($block) ($asm $block))))))

  (define-asm (local $lookup $syntax)
    (syntax-case $syntax ()
      ((_ asm ...)
        (lets
          ($asm (syntaxes->asm $lookup #'(asm ...)))
          (lambda ($block)
            (block+local $block ($asm (empty-block))))))))
)
