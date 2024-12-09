(library (syntax-match)
  (export
    define-pattern-matcher
    pattern-matcher
    pattern-match?
    pattern-match
    syntax-match
    syntax-match?)
  (import (scheme) (syntax) (syntaxes) (fluent) (procedure) (list) (generate) (lets))

  (define-rule-syntax (define-pattern-matcher id expr)
    (begin
      (define-syntax (id $syntax) (syntax-error $syntax "misplaced"))
      (define-property id pattern-matcher expr)))

  (define-lookup-syntax (pattern-matcher $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr pattern body)
        (syntax-case #'pattern (syntax)
          (id
            (and (identifier? #'id) ($lookup #'id #'pattern-matcher))
            (($lookup #'id #'pattern-matcher) $syntax))
          ((id . args)
            (and (identifier? #'id) ($lookup #'id #'pattern-matcher))
            (($lookup #'id #'pattern-matcher) $syntax))
          ((syntax x)
            (identifier? #'x)
            #'(lambda ()
              (lets (x expr) body)))
          ((pattern-1 . pattern-2)
            #'(syntax-case? expr ()
              ((expr-1 . expr-2)
                (opt-lets
                  ($proc
                    (pattern-matcher #'expr-1 pattern-1
                      (pattern-matcher #'expr-2 pattern-2 body)))
                  ($proc)))))
          (underscore
            (syntax=? #'underscore #'_)
            #'(lambda () body))
          (id
            (identifier? #'id)
            #'(syntax-case? expr (id)
              (id (lambda () body))))
          (other
            #'(syntax-case? expr ()
              (other (lambda () body))))))))

  (define-rule-syntax (pattern-match? expr pattern body)
    (lets
      ($proc? (pattern-matcher expr pattern body))
      (and $proc? ($proc?))))

  (define-rule-syntax (pattern-match expr pattern body)
    (lets
      ($proc? (pattern-matcher expr pattern body))
      (if $proc? ($proc?) (syntax-error expr))))

  (define-rule-syntax (syntax-match expr (pattern body) ...)
    (lets
      ($expr expr)
      (app
        (or
          (pattern-matcher $expr pattern body) ...
          (syntax-error $expr)))))

  (define-rule-syntax (syntax-match? expr clause ...)
    (syntax-match expr
      clause ...
      (_ #f)))
)
