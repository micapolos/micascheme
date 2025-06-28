(library (check)
  (export
    checking?
    check
    raises
    works
    define-check->
    define-check-datum->
    check-datum=?
    check-equal?)
  (import (scheme) (identifier))

  (define checking? (make-thread-parameter #f))

  (define-syntax raises
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ (syntax-error $syntax)))))

  (define-syntax works
    (lambda ($syntax)
      (syntax-case $syntax ()
        (_ (syntax-error $syntax)))))

  (meta define (syntax->location-string $syntax)
    (let
      (($annotation (syntax->annotation $syntax)))
      (or
        (and $annotation
          (let
            (($source (annotation-source $annotation)))
            (let-values ((($path $line $column) (locate-source-object-source $source #t #t)))
              (format " source: ~a (~a:~a)\n" $path $line $column))))
        "")))

  (define-syntax check
    (lambda (stx)
      (syntax-case stx (not raises works)
        ((_ (raises body))
          #`(or
            (guard
              ($exception (else #'(void)))
              body
              #f)
            (syntax-error #'body "did not raise")))
        ((_ (raises body ...))
          #'(check (raises (begin body ...))))
        ((_ (works body))
          #`body)
        ((_ (works body ...))
          #'(check (works (begin body ...))))
        ((_ (not (pred arg ...)))
          (let*
            (
              (args (syntax->list #`(arg ...)))
              (tmps (generate-temporaries #`(arg ...)))
              (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
              (ann-string (syntax->location-string stx)))
            #`(parameterize ((checking? #t) (gensym-count 0))
              (let (#,@let-cases)
                (or
                  (not (pred #,@tmps))
                  (error `check
                    (format "\n~a   expr: ~s\n  value: ~s\n"
                      #,ann-string
                      (quote (not (pred arg ...)))
                      (list (quote not) (list (quote pred) #,@tmps)))))))))
        ((_ (pred arg ...))
          (let*
            (
              (args (syntax->list #`(arg ...)))
              (tmps (generate-temporaries #`(arg ...)))
              (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
              (ann-string (syntax->location-string stx)))
            #`(parameterize ((checking? #t) (gensym-count 0))
              (let (#,@let-cases)
                (or
                  (pred #,@tmps)
                  (error `check
                    (format "\n~a   expr: ~s\n  value: ~s\n"
                      #,ann-string
                      (quote (pred arg ...))
                      (list (quote pred) #,@tmps)))))))))))

  (define-syntax (define-check-> $syntax)
    (syntax-case $syntax ()
      ((_ id)
        #`(define-syntax (#,(identifier-append #'id #'check- #'id) $syntax)
          (syntax-case $syntax (raises)
            ((_ (raises (name arg (... ...) input)))
              #`(check
                (raises
                  (#,(identifier-append #'name #'name #'-> #'id) arg (... ...) input))))
            ((_ (name arg (... ...) input) expected)
              #`(check
                (equal?
                  (#,(identifier-append #'name #'name #'-> #'id) arg (... ...) input)
                  expected))))))))

  (define-syntax (define-check-datum-> $syntax)
    (syntax-case $syntax ()
      ((_ id)
        #`(define-syntax (#,(identifier-append #'id #'check- #'id) $syntax)
          (syntax-case $syntax (raises)
            ((_ (raises (name arg (... ...) input)))
              #`(check
                (raises
                  (#,(identifier-append #'name #'name #'-> #'id) arg (... ...) #'input))))
            ((_ (name arg (... ...) input) expected)
              #`(check
                (equal?
                  (syntax->datum (#,(identifier-append #'name #'name #'-> #'id) arg (... ...) #'input))
                  'expected))))))))

  (define-syntax check-datum=?
    (syntax-rules ()
      ((_ a b)
        (check
          (equal?
            (syntax->datum a)
            (syntax->datum b))))))

  (define-syntax check-equal?
    (syntax-rules ()
      ((_ a b)
        (check (equal? a b)))))
)
