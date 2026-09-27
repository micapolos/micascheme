(library (leo3 symbolizer)
  (export
    symbolize
    symbolize-as
    symbolize-is
    symbolize-dashed)
  (import
    (scheme)
    (list)
    (switch)
    (procedure))

  (define (symbolize $symbols)
    (symbolize-as $symbols))

  (define (symbolize-as $symbols)
    (string->symbol
      (apply string-append
        (intercalate
          (map
            (dot symbol->string symbolize-is)
            (splitp (partial symbol=? 'as) $symbols))
          "->"))))

  (define (symbolize-is $symbols)
    (switch $symbols
      ((pair? $pair)
        (case (car $pair)
          ((is)
            (string->symbol
              (string-append
                (symbol->string (symbolize-is (cdr $symbols)))
                "?")))
          (else
            (symbolize-dashed $symbols))))
      ((else $symbols)
        (symbolize-dashed $symbols))))

  (define (symbolize-dashed $symbols)
    (string->symbol
      (apply string-append
        (intercalate
          (map symbol->string $symbols)
          "-"))))
)
