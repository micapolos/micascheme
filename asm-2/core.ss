(library (asm-2 core)
  (export
    (rename (%block block) (%+ +))
    expr db dw)
  (import (micascheme) (asm-2 fragment) (asm-2 block) (asm-2 block-fragment))

  (define-rule-syntax (%block line ...)
    (labeled-block-fragment line ...))

  (define-syntax (expr $syntax)
    (syntax-case $syntax ()
      ; ((_ id)
      ;   (identifier? #'id)
      ;   #'(fragment
      ;     (list #'id)
      ;     (lambda ($lookup) ($lookup #'id))))
      ((_ literal)
        (literal? (datum literal))
        #'(fragment-with (lambda () literal)))
      ((_ fragment)
        #'fragment)))

  (define-rule-syntax (%+ x ...)
    (fragment-map
      (lambda ($exprs)
        (lambda ()
          (apply + (map (lambda ($expr) ($expr)) $exprs))))
      (fragment-append (expr x) ...)))

  (define-rule-syntax (db x ...)
    (fragment-map list->block
      (fragment-append
        (fragment-map
          (lambda ($expr)
            (block 1
              (lambda (_)
                (u8-binary ($expr)))))
          (expr x)) ...)))

  (define-rule-syntax (dw x ...)
    (fragment-map list->block
      (fragment-append
        (fragment-map
          (lambda ($expr)
            (block 2
              (lambda (_)
                (u16-binary ($expr) (endianness little)))))
          (expr x)) ...)))
)
