(library (zx-next scheme demo)
  (export define demo)
  (import
    (except (micascheme) define)
    (zx-next scheme compiler)
    (prefix (zx-next scheme keywords) %)
    (prefix (zx-next demo) %%)
    (prefix (zx-next throw) %%))
  (export (import (zx-next scheme keywords)))

  (define-syntax (define $syntax $lookup)
    (compile-define $lookup $syntax))

  (define-syntax (demo $syntax $lookup)
    (syntax-case $syntax ()
      ((_ x ...)
        (syntax-case (compile-op $lookup #'(%begin x ...)) ()
          ((begin def ... body)
            #`(begin def ...
              (%%demo
                (%%catch body)
                (%%when %%c (%%writeln-error "thrown")))))))))
)
