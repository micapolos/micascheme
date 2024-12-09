(library (syntax-match)
  (export
    define-pattern-matcher
    pattern-matcher
    pattern-match?
    pattern-match
    syntax-match-clause
    define-syntax-match-clause
    syntax-match
    syntax-match?)
  (import (scheme) (syntax) (syntaxes) (fluent) (procedure) (list) (generate) (lets))

  (define-aux-keyword syntax-match-clause)

  (define-rules-syntax
    ((define-syntax-match-clause (id param ...) in body)
      (define-syntax-match-clause id
        (lambda ($syntax)
          (syntax-case $syntax ()
            (((id param ...) in) #'body)))))
    ((define-syntax-match-clause id proc)
      (begin
        (define-syntax (id $syntax) (syntax-error $syntax "misplaced syntax-match-matcher"))
        (define-property id syntax-match-clause proc))))

  ; TODO: Add support for ...
  (define-syntax (syntax-match $syntax)
    (lambda ($lookup)
      (define (transform-clause $syntax)
        (syntax-case $syntax (syntax)
          ((matcher body)
            (transform-clause #`(matcher #t body)))
          (((syntax x) fender body)
            (with-syntax
              ((tmp (generate-identifier #'syntax)))
              #`(tmp fender (let ((x #'tmp)) body))))
          (((id . args) fender body)
            (and (identifier? #'id) ($lookup #'id #'syntax-match-clause))
            (($lookup #'id #'syntax-match-clause) #`((id . args) body)))
          (((id . args) fender body)
            (syntax-case (transform-clause #`(id body)) ()
              ((id-pattern id-fender id-body)
                (syntax-case (transform-clause #`(args id-body)) ()
                  ((args-pattern args-fender args-body)
                    #`(
                      (id-pattern . args-pattern)
                      (and id-fender args-fender)
                      args-body))))))
          ((underscore fender body)
            (syntax=? #'underscore #'_)
            #'(_ fender body))
          ((id fender body)
            (identifier? #'id)
            (with-syntax
              ((tmp (generate-identifier #'id)))
              #`(tmp
                (and (identifier? #'tmp) (free-identifier=? #'tmp #,#'#`id) fender)
                body)))
          (other #'other)))

      (syntax-case $syntax ()
        ((_ expr clause ...)
          #`(syntax-case expr ()
            #,@(map transform-clause
              (syntax->list #'(clause ...))))))))

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

  (define-rule-syntax (syntax-match? clause ...)
    (syntax-match clause ... (_ #f)))
)
