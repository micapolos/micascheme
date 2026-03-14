(library (leo load)
  (export load-leo-program)
  (import
    (micascheme)
    (getter)
    (leo leo)
    (leo getter))

  (define (load-leo-program $path)
    (leo (load-program $path)))
)
