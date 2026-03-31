(library (leo document)
  (export document example)
  (import
    (except (scheme) write)
    (syntaxes)
    (procedure)
    (only (leo write) write))

  (define-rules-syntaxes
    ((document x ...)
      (begin (write 'x) ...))
    ((example x ...)
      `(example
        (program x ...)
        (result ,(run x ...)))))
)
