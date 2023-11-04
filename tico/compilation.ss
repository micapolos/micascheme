(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation

    literal->compilation
    datum->compilation)
  (import
    (micascheme)
    (tico constant))

  (data (compilation datum evaluation))

  (define (literal->compilation $literal)
    (compilation $literal (constant $literal)))

  (define (datum->compilation $datum)
    (compilation $datum (datum->constant $datum)))
)
