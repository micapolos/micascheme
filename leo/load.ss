(library (leo load)
  (export load-leo-program)
  (import
    (micascheme)
    (getter)
    (leo leo)
    (leo getter))

  (define (load-leo-program $path)
    (with-leo
      (eval
        `(top-level-program
          ,@(getter-load! line-annotations-getter $path))
        (copy-environment (scheme-environment) #t))))
)
