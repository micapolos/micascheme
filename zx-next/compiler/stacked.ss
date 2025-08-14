(library (zx-next compiler stacked)
  (export stacked define-stacked-op)
  (import
    (micascheme)
    (zx-next compiler stacked-asm))

  (define-rules-syntax
    ((define-stacked-op id x)
      (define-syntax x
        (make-compile-time-value
          #`(stacked-op #,x))))
    ((define-stacked-op (id $syntax) body)
      (define-stacked id
        (lambda ($syntax)
          body))))

  (define-syntax (stacked $syntax $lookup)
    (syntax-case $syntax ()
      ((_ regs x ...)
        (stacked-asm $lookup #'(regs x ...)))))
)
