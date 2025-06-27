(library (keyword)
  (export
    keyword-append
    keyword-replace)
  (import
    (scheme)
    (syntax)
    (identifier))

  (define-case-syntax (keyword-append part ...)
    (apply identifier-append #'keyword-append #'(part ...)))

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
