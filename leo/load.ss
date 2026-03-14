(library (leo load)
  (export load-leo)
  (import
    (micascheme)
    (getter)
    (leo leo)
    (leo getter))

  (define (load-leo $path)
    (leo (load $path)))
)
