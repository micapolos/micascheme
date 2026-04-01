(library (leo display)
  (export display display-string)
  (import
    (rename
      (except (scheme) display write)
      (display-string %display-string))
    (leo write)
    (leo quotify))

  (define display
    (case-lambda
      ((object)
        (display object (current-output-port)))
      ((object port)
        (parameterize ((quotify-for-display? #t))
          (write object port)))))

  (define display-string %display-string)
)
