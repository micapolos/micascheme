(library (asm-2 lang)
  (export asm check-asm define)
  (import
    (rename (micascheme) (define %define))
    (asm-2 typed))
  (export
    (import (only (micascheme) string lambda))
    (import (only (asm-2 block) block))
    (import (only (asm-2 typed) void type boolean integer char function typed macro asm-bytevector label db dw binary)))

  (define-syntax (define $syntax $lookup)
    (syntax-case $syntax (typed)
      ((_ id (typed type expr))
        #`(define-typed id (typed type expr)))
      ((_ (id . params) body)
        #`(define id (lambda params body)))
      ((_ id expr)
        (syntax-case (syntax->typed $lookup #'expr) (typed)
          ((typed type value)
            #`(begin
              (%define untyped value)
              (define-typed id
                (lambda ($lookup $syntax)
                  (syntax-case $syntax ()
                    (id (identifier? #'id)
                      #`(typed type untyped)))))))))))

  (define-rule-syntax (check-asm in out)
    (check (equal? (asm in) (asm out))))

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (syntax-case (syntax->typed $lookup #'expr) (typed)
          ((typed type expr)
            #``(typed type ,expr))))))
)
