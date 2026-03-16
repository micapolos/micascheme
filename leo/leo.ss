(library (leo leo)
  (export leo)
  (import
    (language)
    (leo language))

  (define-language leo
    (language-append leo-language scheme-language))
)
