(library (keyword)
  (export
    keyword-append
    keyword-replace)
  (import
    (scheme)
    (syntax)
    (identifier))

  (define-syntax (keyword-append $syntax)
    (syntax-case $syntax ()
      ((id part ...)
        (apply
          identifier-append
          (syntax id)
          (syntaxes part ...)))))

  (define-rule-syntax (keyword-replace old new body)
    (let-syntax
      ((old
        (lambda ($syntax)
          (syntax-case $syntax ()
            (old
              (identifier? #'old)
              #'new)
            ((old . rest)
              #'(new . rest))))))
      body))
)
