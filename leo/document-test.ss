(import
  (scheme)
  (check)
  (leo document)
  (only (micascheme) lines-string))

(check
  (equal?
    (example (+ 2 2))
    '(example
      (program (+ 2 2))
      (result 4))))

(check
  (equal?
    (call-with-string-output-port
      (lambda (port)
        (parameterize
          ((current-output-port port))
          (document hello))))
    (lines-string "hello")))
