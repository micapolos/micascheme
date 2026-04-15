(library (keyword)
  (export
    keyword
    keyword?
    keywords?
    free-keyword?
    free-keywords?
    keyword-append
    keyword-replace
    keyword...?)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (identifier))

  (define-rules-syntaxes
    ((keyword x)
      #'x)
    ((keyword? x)
      (identifier? #'x))
    ((keywords? x ...)
      (begin (keyword? x) ...))
    ((free-keyword? x)
      (and
        (keyword? x)
        (free-identifier=? #'x (datum->syntax #'x 'x))))
    ((free-keywords? x ...)
      (begin (free-keyword? x) ...))
    ((keyword-append tpl part ...)
      (identifier-append #'tpl #'part ...)))

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

  (define (identifier...? $id)
    (free-identifier=? $id #'(... ...)))

  (define-rule-syntax (keyword...? k)
    (identifier...? #'k))
)
