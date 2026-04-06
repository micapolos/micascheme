(library (keyword)
  (export
    keyword
    keyword?
    free-keyword?
    keyword-append
    keyword-replace
    keyword...?)
  (import
    (scheme)
    (syntax)
    (identifier))

  (define-rule-syntax (keyword? x)
    (identifier? #'x))

  (define-rule-syntax (keyword x)
    #'x)

  (define-rule-syntax (free-keyword? x)
    (and
      (keyword? x)
      (free-identifier=? #'x (literal->syntax 'x))))

  (define-rule-syntax (keyword-append tpl part ...)
    (identifier-append #'tpl #'part ...))

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
