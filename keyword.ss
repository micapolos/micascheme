(library (keyword)
  (export
    keyword
    keyword?
    keyword=?
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

  (define keyword=? free-identifier=?)

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
