(library (typed lang)
  (export
    tt
    any any-any-lambda any-type
    any-boolean any-string any-number any-syntax any-lambda
    type typeof
    syntax lambda
    define)
  (import
    (micascheme)
    (any)
    (typed type)
    (typed typed)
    (typed syntax)
    (typed keywords)
    (typed phased))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ body ...)
        #`(begin
          #,@(flatten
            (map-with
              ($body (syntaxes body ...))
              (syntax-case $body (define)
                ((define name body)
                  (identifier? #'name)
                  (lets
                    ($typed
                      (syntax->typed 0
                        (lambda ($identifier)
                          ($lookup $identifier #'phased))
                        #'body))
                    (list
                      #`(define name #,(typed-value $typed))
                      #`(define-property name phased
                        (phased 0
                          (typed
                            #,(type->syntax (typed-type $typed))
                            #'name))))))
                ((define (name param ...) body)
                  (identifier? #'name)
                  (lets
                    ($typed
                      (syntax->typed 0
                        (lambda ($identifier)
                          ($lookup $identifier #'phased))
                        #'(lambda (param ...) body)))
                    (list
                      #`(define name #,(typed-value $typed))
                      #`(define-property name phased
                        (phased 0
                          (typed
                            #,(type->syntax (typed-type $typed))
                            #'name))))))
                (expr
                  (list
                    (typed-value
                      (syntax->typed 0
                        (lambda ($identifier)
                          ($lookup $identifier #'phased))
                        #'expr)))))))))))
)
