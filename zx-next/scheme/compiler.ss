(library (zx-next scheme compiler)
  (export
    scoped
    byte word
    byte+ byte-
    scoped-expr->asm
    check-scoped-expr->asm)
  (import
    (micascheme)
    (prefix (zx-next core) %)
    (prefix (zx-next scheme primitives) %))

  (define-keywords scoped byte word byte+ byte-)

  (define (scoped-expr->asm $lookup $scoped-expr)
    (syntax-case $scoped-expr (scoped)
      ((scoped params locals expr)
        (let ()
          (define-rule-syntax (recurse x)
            (scoped-expr->asm $lookup #'(scoped params locals x)))
          (define-rule-syntax (op-2 id a b)
            #`(%begin #,(recurse b) #,(recurse a) (id)))
          (syntax-case #'expr (byte word byte+ byte- let)
            ((byte n) #'(%push-byte n))
            ((word n) #'(%push-word n))
            ((byte+ a b) (op-2 %byte-add a b))
            ((byte- a b) (op-2 %byte-sub a b)))))))

  (define-rule-syntax (check-scoped-expr->asm lookup expr asm)
    (check (equal? (syntax->datum (scoped-expr->asm lookup #'expr)) 'asm)))
)
