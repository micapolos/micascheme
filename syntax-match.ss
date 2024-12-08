(library (syntax-match)
  (export
    syntax-match-clause
    define-syntax-match-clause
    syntax-match
    syntax-match?)
  (import (scheme) (syntax) (syntaxes) (generate) (lets))

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
            (lets
              ($tmp (generate-identifier #'syntax))
              #`(#,$tmp fender (let ((x #'#,$tmp)) body))))
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
            (lets
              ($tmp (generate-identifier #'id))
              #`(#,$tmp
                (and (identifier? #'#,$tmp) (free-identifier=? #'#,$tmp #,#'#`id) fender)
                body)))
          (other #'other)))

      (syntax-case $syntax ()
        ((_ expr clause ...)
          #`(syntax-case expr ()
            #,@(map transform-clause
              (syntax->list #'(clause ...))))))))

  (define-rule-syntax (syntax-match? clause ...)
    (syntax-match clause ... (_ #f)))
)
