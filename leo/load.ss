(library (leo load)
  (export load-leo-program)
  (import
    (micascheme)
    (getter)
    (leo read)
    (leo getter))

  (define (load-leo-program $path)
    (with-leo-read
      (eval
        `(top-level-program
          (import (leo lang))
          ,@(getter-load! line-annotations-getter $path))
        (copy-environment (scheme-environment) #t))))
)
