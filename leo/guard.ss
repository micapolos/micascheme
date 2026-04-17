(library (leo guard)
  (export guard)
  (import
    (rename (scheme) (guard %guard))
    (syntax-keywords)
    (syntaxes)
    (leo in))

  (define-rules-syntax
    (keywords when else in)

    (
      (guard
        (when (id test?) a as ...)
        ...
        (else (eid b bs ...))
        (in c cs ...))
      (%guard
        (var
          ((test? var) (let ((id var)) a as ...))
          ...
          (else (let ((eid var)) b bs ...)))
        c cs ...))

    (
      (guard
        (when (id test?) a as ...)
        ...
        (in c cs ...))
      (%guard
        (var
          ((test? var) (let ((id var)) a as ...))
          ...)
        c cs ...)))
)
