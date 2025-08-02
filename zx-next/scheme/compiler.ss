(library (zx-next scheme compiler)
  (export
    expr->asm
    stmt->asm
    check-expr->asm
    check-stmt->asm)
  (import
    (micascheme)
    (syntax lookup)
    (prefix (zx-next scheme compiler-keywords) %%)
    (prefix (zx-next core) %)
    (prefix (zx-next scheme primitives) %))

  (define (stmt->asm $lookup $stmt)
    (syntax-case $stmt (%%write %%write-stack %%begin %%lets)
      ((%%write expr)
        #`(%begin
          #,(expr->asm $lookup #'expr)
          (%write-value)))
      ((%%write-stack)
        #`(%write-stack))
      ((%%begin stmt ...)
        #`(%begin #,@(map (partial stmt->asm $lookup) #'(stmt ...))))
      ((%%lets expr ... stmt)
        #`(%begin
          #,@(map (partial expr->asm $lookup) #'(expr ...))
          #,(stmt->asm $lookup #'stmt)
          #,@(map (constant-procedure #'(%pop-value)) #'(expr ...))))))

  (define (expr->asm $lookup $expr)
    (let ()
      (define-rule-syntax (recurse x)
        (expr->asm $lookup #'x))
      (define-rule-syntax (op-2 id a b)
        #`(%begin #,(recurse b) #,(recurse a) (id)))
      (syntax-case $expr (%%top %%byte %%word %%byte+ %%byte- %%lets)
        (#t #'(%push-true))
        (#f #'(%push-false))
        (() #'(%push-null))
        ((%%top index) #'(%dup-value index))
        ((%%byte n) #'(%push-byte n))
        ((%%word n) #'(%push-word n))
        ((%%byte+ a b) (op-2 %byte-add a b))
        ((%%byte- a b) (op-2 %byte-sub a b))
        ((%%byte* a b) (op-2 %byte-mul a b))
        ((%%lets expr ... body)
          #`(%begin
            #,@(map (partial expr->asm $lookup) #'(expr ...))
            #,(recurse body)
            #,@(map (constant-procedure #'(%pop-value)) #'(expr ...)))))))

  (define-rule-syntax (check-expr->asm lookup expr asm)
    (check (equal? (syntax->datum (expr->asm lookup #'expr)) 'asm)))

  (define-rule-syntax (check-stmt->asm lookup expr asm)
    (check (equal? (syntax->datum (stmt->asm lookup #'expr)) 'asm)))
)
