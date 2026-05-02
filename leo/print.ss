(library (leo print)
  (export print print-line)
  (import
    (scheme)
    (lets)
    (procedure)
    (code)
    (leo sentence)
    (leo code))

  (define print
    (case-lambda
      ((x)
        (print x (current-output-port)))
      ((x port)
        (parameterize ((pretty-write? #t))
          (put-string port (code-string (block-code x)))))))

  (define print-line
    (case-lambda
      ((x)
        (print-line x (current-output-port)))
      ((x port)
        (lets
          ($port port)
          (parameterize ((pretty-write? #t))
            (put-string $port (code-string (line-code x)))
            (newline $port))))))
)
