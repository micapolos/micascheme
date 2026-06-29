(library (asm8 lang)
  (export
    u8.push u8.inc u8.dec u8.add u8.sub u8.mul
    asm8)
  (import
    (scheme)
    (generate)
    (stack)
    (syntax)
    (asm8 compiler)
    (asm8 runtime))

  (define-keywords u8.push u8.inc u8.dec u8.add u8.sub u8.mul)

  (meta define (syntax->op $syntax)
    (syntax-case $syntax (u8.push u8.inc u8.dec u8.add u8.sub u8.mul)
      ((u8.push u8) (op 0 1 (lambda () (datum u8))))
      (u8.inc (op 1 1 (lambda ($0) `(u8+ ,$0 1))))
      (u8.dec (op 1 1 (lambda ($0) `(u8- ,$0 1))))
      (u8.add (op 2 1 (lambda ($0 $1) `(u8+ ,$0 ,$1))))
      (u8.sub (op 2 1 (lambda ($0 $1) `(u8- ,$0 ,$1))))
      (u8.mul (op 2 1 (lambda ($0 $1) `(u8* ,$0 ,$1))))
      (else (syntax-error $syntax "invalid op"))))

  (define-syntax (asm8 $syntax)
    (syntax-case $syntax ()
      ((_ op ...)
        (datum->syntax #'+
          (compile-stack
            (compile-ops
              gensym
              (stack)
              (map syntax->op #'(op ...))))))))
)
