(library (zx-next scheme checker)
  (export
    scoped
    byte word
    byte+ byte-
    scoped-expr->asm
    check-scoped-expr->asm)
  (import
    (micascheme)
    (syntax lookup)
    (prefix (zx-next core) %)
    (prefix (zx-next scheme primitives) %))

  (define-keywords scoped byte word byte+ byte-)

  (define (scoped-expr->asm $lookup $scoped-expr)
    (syntax-case $scoped-expr (scoped)
      ((scoped (arg ...) (local ...) expr)
        (let ()
          (define-rule-syntax (index? ids id)
            (lets?
              ($indexed (map-find-indexed (partial free-identifier=? #'id) #'ids))
              (indexed-index $indexed)))
          (define-rule-syntax (recurse x)
            (scoped-expr->asm $lookup #'(scoped (arg (... ...)) (local (... ...)) x)))
          (define-rule-syntax (op-2 id a b)
            #`(%begin #,(recurse b) #,(recurse a) (id)))
          (syntax-case #'expr (byte word byte+ byte- lets)
            (id (identifier? #'id)
              (switch (index? (local ...) id)
                ((integer? $index) #`(%dup-value #,$index))
                ((false? _) (lookup-ref $lookup #'id))))
            ((byte n) #'(%push-byte n))
            ((word n) #'(%push-word n))
            ((byte+ a b) (op-2 %byte-add a b))
            ((byte- a b) (op-2 %byte-sub a b))
            ((lets (id expr) ... body)
              #`(%begin
                #,@(map-with
                  ($expr #'(expr ...))
                  (scoped-expr->asm $lookup #`(scoped (arg ...) (local ...) #,$expr)))
                #,(scoped-expr->asm $lookup
                  #`(scoped
                    (arg ...)
                    (id ... local ...)
                    body))
                #,@(map-with
                  (_ #'(id ...))
                  #'(%pop-value)))))))))

  (define-rule-syntax (check-scoped-expr->asm lookup expr asm)
    (check (equal? (syntax->datum (scoped-expr->asm lookup #'expr)) 'asm)))
)
