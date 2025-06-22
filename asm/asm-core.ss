(library (asm asm-core)
  (export label db dw block local import)
  (import
    (rename (micascheme) (import %import))
    (asm asm)
    (only (asm block) empty-block block+binary-syntax block+label block-bind block+import)
    (only (asm binary) db-binary dw-binary)
    (only (binary) binary-append))

  (define-asm (db $lookup $syntax)
    (syntax-case $syntax ()
      ((_ expr ...)
        (lambda ($block)
          (block+binary-syntax $block (length #'(expr ...))
            #'(binary-append (db-binary expr) ...))))))

  (define-asm (dw $lookup $syntax)
    (syntax-case $syntax ()
      ((_ expr ...)
        (lambda ($block)
          (block+binary-syntax $block (* 2 (length #'(expr ...) ))
            #'(binary-append (dw-binary expr) ...))))))

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
            (block-bind $block (lambda ($block) ($asm $block))))))))

  (define-asm (import $lookup $syntax)
    (syntax-case $syntax ()
      ((id (path ...) ...)
        (lambda ($block)
          (fold-left
            (lambda ($block $path)
              (block+import $block (syntax->datum $path)
                (lambda ($block)
                  (lets
                    ($path
                      (string-append
                        (apply string-append
                          (intercalate (map symbol->string (syntax->datum $path)) "/"))
                        ".ss"))
                    ($syntaxes (load-syntax-list #'id $path))
                    (app (syntaxes->asm $lookup $syntaxes) $block)))))
            $block
            #'((path ...) ...))))))
)
