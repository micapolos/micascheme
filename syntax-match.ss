(library (syntax-match)
  (export
    define-pattern-match?
    pattern-match?
    syntax-match?
    syntax-match)
  (import (scheme) (syntax) (syntaxes) (fluent) (procedure) (list) (generate) (lets) (data))

  (define-rules-syntax
    ((define-pattern-match? id expr)
      (begin
        (define-syntax (id $syntax) (syntax-error $syntax "misplaced"))
        (define-property id pattern-match? expr)))
    ((define-pattern-match? (id param ...) (expr body) match)
      (define-pattern-match? id
        (syntax-rules ()
          ((_ expr (_ param ...) body)
            match)))))

  (define-lookup-syntax (pattern-match? $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr pattern body)
        (syntax-case #'pattern (syntax)
          (id
            (and (identifier? #'id) ($lookup #'id #'pattern-match?))
            (($lookup #'id #'pattern-match?) $syntax))
          ((id . args)
            (and (identifier? #'id) ($lookup #'id #'pattern-match?))
            (($lookup #'id #'pattern-match?) $syntax))
          ((syntax x)
            (identifier? #'x)
            #'(lets (x expr) body))
          ((pattern-1 . pattern-2)
            #'(syntax-case? expr ()
              ((expr-1 . expr-2)
                (pattern-match? #'expr-1 pattern-1
                  (pattern-match? #'expr-2 pattern-2 body)))))
          (underscore
            (syntax=? #'underscore #'_)
            #'body)
          (id
            (identifier? #'id)
            #'(syntax-case? expr (id)
              (id body)))
          (other
            #'(syntax-case? expr ()
              (other body)))))))

  (define-rule-syntax (syntax-match? expr (pattern body) ...)
    (lets ($expr expr)
      (or (pattern-match? $expr pattern body) ...)))

  (define-rule-syntax (syntax-match expr clause ...)
    (syntax-match? expr
      clause ...
      (#'other (syntax-error other))))
)
