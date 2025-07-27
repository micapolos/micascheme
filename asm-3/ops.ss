(library (asm-3 ops)
  (export dup ds)
  (import
    (asm-3 base)
    (only (asm-3 lang) define-op-syntax define-ops))

  (define-op-syntax (dup $syntax)
    (syntax-case $syntax ()
      ((_ n op ...)
        (nonnegative-integer? (datum n))
        #`(begin
          #,@(flatten
            (make-list (datum n) #'(op ...)))))))

  (define-ops
    ((ds size u8) (dup size (db u8)))
    ((ds size) (ds size 0)))
)
