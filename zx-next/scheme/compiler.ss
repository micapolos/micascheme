(library (zx-next scheme compiler)
  (export
    compile-expression
    check-compile-expression)
  (import
    (rename (micascheme)
      (and %and)
      (or %or)
      (xor %xor)
      (pop %pop)
      (push %push))
    (syntax lookup)
    (u)
    (zx-next core)
    (zx-next scheme primitives))

  (define (compile-expression $lookup $syntax)
    (syntax-case $syntax ()
      (((param ...) (local ...) stack-offset body)
        (syntax-case #'body (u8 u16 +)
          ((add u8 a n)
            (u8? (datum n))
            (syntax-case (compile-expression $lookup #'((param ...) (local ...) stack-offset b)) ()
              ((type-b locals-b? asm-b ...)
                (syntax-case (compile-expression $lookup #'((param ...) (#f local ...) 0 a)) ()
                  ((type-a locals-a? asm-a ...)
                    (syntax-case #'type-a (u8)
                      (u8
                        (syntax-case #'type-b (u8)
                          (u8
                            #`(u8
                              #,(and (datum locals-a?) (datum locals-b?))
                              asm-b ...
                              asm-a ...
                              (%byte-add)))
                          (_
                            (syntax-error #'b "not u8"))))
                      (u16
                        (syntax-case #'type-b (u16)
                          (u16
                            #`(u16
                              #,(and (datum locals-a?) (datum locals-b?))
                              asm-b ...
                              asm-a ...
                              %word-add))
                          (_
                            (syntax-error #'b "not u16"))))
                      (_
                        (syntax-error #'a "not additive"))))))))))))

  (define-rule-syntax (check-compile-expression in out)
    (check
      (equal?
        (syntax->datum (compile-expression (empty-lookup) #'(() () 0 in)))
        'out)))
)
