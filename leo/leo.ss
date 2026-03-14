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

  (define-rule-syntax (leo x xs ...)
    (begin
      (invoke-library '(leo scheme))
      (with-language leo-language x xs ...)))
)
