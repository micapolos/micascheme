(library (leo guard)
  (export guard)
  (import
    (rename (scheme) (guard %guard))
    (syntax-keywords)
    (syntaxes))

  (define-rules-syntax
    (keywords when else)

    ((guard (var (when test a as ...) ... (else b bs ...)) c cs ...)
      (%guard (var (test a as ...) ... (else b bs ...)) c cs ...))

    ((guard (var (when test a as ...) ...) b bs ...)
      (%guard (var (test a as ...) ...) b bs ...)))
)
