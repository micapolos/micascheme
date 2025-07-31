(library (asm ops)
  (export dup ds)
  (import
    (asm base)
    (only (asm lang) define-op-syntax define-ops db))

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
