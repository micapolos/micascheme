(library (tico term)
  (export
    term term? term-type term-compilation

    literal->term
    type-datum->term)
  (import
    (micascheme)
    (tico datum)
    (tico type)
    (tico constant)
    (tico variable)
    (tico compilation))

  (data (term type compilation))

  (define (literal->term $literal)
    (term
      (literal-type $literal)
      (literal->compilation $literal)))

  (define (type-datum->term $type $datum)
    (term $type (datum->compilation $datum)))
)
