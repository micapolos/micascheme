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

  (define-rule-syntax (leo body ...)
    (begin
      (invoke-library '(leo scheme))
      (parameterize
        ((current-languages (push (current-languages) leo-language)))
        (with-current-languages body ...))))
)
