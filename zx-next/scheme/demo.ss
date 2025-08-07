(library (zx-next scheme lang)
  (export define demo)
  (import
    (micascheme)
    (zx-next scheme compiler)
    (prefix (zx-next scheme keywords) %)
    (prefix (zx-next demo) %%))
  (export (import (zx-next scheme keywords)))

  (define-syntax (define $lookup $syntax)
    (compile-define $lookup $syntax))

  (define-syntax (demo $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        #`(begin
          #,@(map (partial compile-expression $lookup) #'(x ...))))))
)
