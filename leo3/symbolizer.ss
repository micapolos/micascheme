(library (leo3 symbolizer)
  (export
    symbolize
    symbolize-as
    symbolize-dashed)
  (import
    (scheme)
    (list)
    (procedure))

  (define (symbolize $symbols)
    (symbolize-as $symbols))

  (define (symbolize-as $symbols)
    (string->symbol
      (apply string-append
        (intercalate
          (map
            (dot symbol->string symbolize-dashed)
            (splitp (partial symbol=? 'as) $symbols))
          "->"))))

  (define (symbolize-dashed $symbols)
    (string->symbol
      (apply string-append
        (intercalate
          (map symbol->string $symbols)
          "-"))))
)
