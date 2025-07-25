(library (asm-3 ops)
  (export dup)
  (import
    (asm-3 base)
    (only (asm-3 lang) define-op-syntax))

  (define-op-syntax (dup $syntax)
    (syntax-case $syntax ()
      ((_ n op ...)
        (nonnegative-integer? (datum n))
        #`(begin
          #,@(flatten
            (make-list (datum n) #'(op ...)))))))
)
