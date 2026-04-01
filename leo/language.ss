(library (leo language)
  (export leo-language)
  (import
    (scheme)
    (language)
    (leo read)
    (leo expand))

  (define leo-language
    (language (list "leo") make-leo-read leo-expand))
)
