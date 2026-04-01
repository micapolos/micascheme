(library (leo write)
  (export write write-line)
  (import
    (except (scheme) write)
    (lets)
    (procedure)
    (code)
    (leo code))

  (define write
    (case-lambda
      ((x)
        (write x (current-output-port)))
      ((x port)
        (put-string port (code-string (block-code x))))))

  (define write-line
    (case-lambda
      ((x)
        (write-line x (current-output-port)))
      ((x port)
        (lets
          ($port port)
          (run
            (put-string $port (code-string (line-code x)))
            (newline $port))))))
)
