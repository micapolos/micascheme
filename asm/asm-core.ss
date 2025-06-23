(library (asm asm-core)
  (export label db dw ds block local import import-base reverse string-c)
  (import
    (rename (micascheme) (import %import) (reverse %reverse) (bytevector %bytevector) (make-bytevector %make-bytevector))
    (asm asm)
    (only (asm block) empty-block block+binary-syntax block+label block-bind block+import block-import-base block-with-import-base)
    (only (asm binary) db-binary dw-binary)
    (only (binary) binary-append bytevector-binary)
    (only (asm std) bytevector make-bytevector))

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

  (define-asm (ds $lookup $syntax)
    (syntax-case $syntax ()
      ((_ size )
        (syntax->asm $lookup #'(ds size 0)))
      ((_ size value)
        (and (integer? (datum size)) (integer? (datum value)))
        (lambda ($block)
          (block+binary-syntax $block (datum size)
            #`(bytevector-binary
              (make-bytevector size value)))))))

  (define-asm (string-c $lookup $syntax)
    (syntax-case $syntax ()
      ((_ s)
        (string? (datum s))
        (lets
          ($bytevector (string->utf8 (datum s)))
          (cond
            ((= (bytevector-length $bytevector) 0)
              (syntax-error $syntax "empty string"))
            (else
              (lets
                ($length (bytevector-length $bytevector))
                ($last-index (- $length 1))
                ($u8-list (bytevector->u8-list $bytevector))
                (cond
                  ((exists (lambda ($u8) (not (zero? (bitwise-and $u8 #x80)))) $u8-list)
                    (syntax-error $syntax "non-ascii chars"))
                  (else
                    (lambda ($block)
                      (block+binary-syntax $block $length
                        #`(bytevector-binary
                          (bytevector
                            #,@(map-with
                              ($index (iota $length))
                              ($u7 $u8-list)
                              (literal->syntax
                                (if (= $index $last-index)
                                  (bitwise-ior $u7 #x80)
                                  $u7))))))))))))))))

  (define-asm (label $lookup $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (lambda ($block)
          (block+label $block #'id)))))

  (define-asm (block $lookup $syntax)
    (syntax-case $syntax ()
      ((_ asm ...)
        (syntaxes->asm $lookup #'(asm ...)))))

  (define-asm (local $lookup $syntax)
    (syntax-case $syntax ()
      ((_ asm ...)
        (lets
          ($asm (syntaxes->asm $lookup #'(asm ...)))
          (lambda ($block)
            (block-bind $block (lambda ($block) ($asm $block))))))))

  (define-asm (reverse $lookup $syntax)
    (syntax-case $syntax ()
      ((_ asm ...)
        (syntaxes->asm $lookup (%reverse #'(asm ...))))))

  (define-asm (import-base $lookup $syntax)
    (syntax-case $syntax ()
      ((id (path ...))
        (lambda ($block)
          (block-with-import-base $block
            #'(path ...))))))

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
                          (intercalate
                            (map symbol->string
                              (append
                                (syntax->datum (block-import-base $block))
                                (syntax->datum $path)))
                            "/"))
                        ".ss"))
                    ($syntaxes (load-syntax-list #'id $path))
                    (app (syntaxes->asm $lookup $syntaxes) $block)))))
            $block
            #'((path ...) ...))))))
)
