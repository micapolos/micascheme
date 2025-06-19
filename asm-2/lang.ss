(library (asm-2 lang)
  (export asm define)
  (import
    (rename (micascheme) (define %define))
    (asm-2 typed))
  (export
    (import (only (micascheme) string lambda))
    (import (only (asm-2 typed) void type boolean integer char procedure define-typed typed)))

  (define-syntax (define $syntax $lookup)
    (syntax-case $syntax ()
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

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed $lookup #'expr)))))
)
