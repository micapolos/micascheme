(library (zx-next scheme compiler)
  (export
    expr->asm
    check-expr->asm)
  (import
    (micascheme)
    (syntax lookup)
    (prefix (zx-next scheme compiler-keywords) %%)
    (prefix (zx-next core) %)
    (prefix (zx-next scheme primitives) %))

  (define (expr->asm $lookup $expr)
    (let ()
      (define-rule-syntax (recurse x)
        (expr->asm $lookup #'x))
      (define-rule-syntax (op-2 id a b)
        #`(%begin #,(recurse b) #,(recurse a) (id)))
      (syntax-case $expr (%%top %%byte %%word %%byte+ %%byte- %%lets)
        ((%%top index) #'(%dup-value index))
        ((%%byte n) #'(%push-byte n))
        ((%%word n) #'(%push-word n))
        ((%%byte+ a b) (op-2 %byte-add a b))
        ((%%byte- a b) (op-2 %byte-sub a b))
        ((%%lets expr ... body)
          #`(%begin
            #,@(map (partial expr->asm $lookup) #'(expr ...))
            #,(recurse body)
            #,@(map (constant-procedure #'(%pop-value)) #'(expr ...)))))))

  (define-rule-syntax (check-expr->asm lookup expr asm)
    (check (equal? (syntax->datum (expr->asm lookup #'expr)) 'asm)))
)
