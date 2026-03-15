(library (leo leo)
  (export leo)
  (import
    (micascheme)
    (leo expand)
    (leo read)
    (leo path)
    (leo source-file-descriptor)
    (language)
    (leo language))

  (define-language leo
    (language-append leo-language scheme-language))
)
