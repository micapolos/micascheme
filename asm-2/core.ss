(library (asm-2 core)
  (export
    (rename (%block block) (%+ +))
    wrap db dw)
  (import
    (micascheme)
    (asm-2 fragment)
    (asm-2 block)
    (asm-2 block-fragment))

  (define-rule-syntax (%block line ...)
    (labeled-block-fragment line ...))

  (define-syntax (wrap $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #'(fragment-with (dep id) (dep id)))
      ((_ other)
        #'(wrap-fragment other))))

  (define-rule-syntax (%+ x ...)
    (fragment-map
      (partial apply +)
      (fragment-append (wrap x) ...)))

  (define-rule-syntax (db x ...)
    (fragment-map list->block
      (fragment-append
        (fragment-map
          (lambda ($expr)
            (block-with 1
              (u8-binary $expr)))
          (wrap x)) ...)))

  (define-rule-syntax (dw x ...)
    (fragment-map list->block
      (fragment-append
        (fragment-map
          (lambda ($expr)
            (block-with 2
              (u16-binary $expr (endianness little))))
          (wrap x)) ...)))
)
