(library (leo test)
  (export test-path test)
  (import
    (except (scheme) write)
    (leo write)
    (leo load)
    (keyword)
    (syntaxes))

  (define-rules-syntaxes
    ((test-path (id x))
      (string-append (symbol->string 'id)
        "/"
        (test-path x)))
    ((test-path id)
      (string-append
        (symbol->string 'id)
        "-test.leo"))
    ((test x ...)
      (begin
        (begin
          (write '(testing x))
          (load-leo-program (test-path x)))
        ...)))
)
