(library (asm lang)
  (export
    asm
    check-asm
    define
    define-primitive
    define-primitives
    define-macro)
  (import
    (rename (micascheme) (define %define))
    (asm typed))
  (export
    (import (only (micascheme) string let lambda syntax binary-append binary->bytevector keywords define-keywords))
    (import (only (asm block) block))
    (import (only (asm binary) db-binary dw-binary))
    (import (only (asm u) u2 u3 u8 u16))
    (import (asm core))
    (import (only (asm typed) void type boolean integer char function typed asm-binary label db dw binary assembly define-asm local shadow)))

  (define-syntax (define $syntax $lookup)
    (syntax-case $syntax (typed)
      ((_ id (typed type expr))
        #`(define-typed id (typed type expr)))
      ((_ (id . params) body)
        #`(define id (lambda params body)))
      ((_ id expr)
        (lets
          ((typed $type $value) (syntax->typed $lookup #'expr))
          #`(begin
            (%define untyped #,$value)
            (define-typed id
              (lambda ($lookup $syntax)
                (syntax-case $syntax ()
                  (id
                    (identifier? #'id)
                    (typed #'#,$type #'untyped))
                  (other
                    (syntax->typed-noexpand $lookup #'other))))))))))

  (define-rules-syntax
    ((define-primitive id type prim)
      (define id (typed type ($primitive 3 prim))))
    ((define-primitive id type)
      (define-primitive id type id)))

  (define-rule-syntax (define-primitives (args ...) ...)
    (begin
      (define-primitive args ...) ...))

  (define-rule-syntax (define-macro (id arg ...) body)
    (define-typed (id $lookup $syntax)
      (syntax->typed $lookup
        (syntax-case $syntax ()
          ((id arg ...) #'body)))))

  (define-rule-syntax (check-asm in out)
    (check (equal? (asm in) (asm out))))

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value (syntax->typed $lookup #'expr)))))
)
