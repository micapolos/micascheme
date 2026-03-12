(library (leo leo)
  (export leo)
  (import
    (micascheme)
    (leo expand)
    (leo read)
    (leo path)
    (leo source-file-descriptor))

  (define-rule-syntax (leo body ...)
    (begin
      (invoke-library '(leo scheme))
      (parameterize
        (
          (make-read-handler make-leo-read)
          (current-expand leo-expand)
          (library-extensions leo-library-extensions))
        body ...)))
)
