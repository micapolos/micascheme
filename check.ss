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

  (define (raises? $proc)
    (call/cc
      (lambda (cont)
        (with-exception-handler
          (lambda (_) (cont #t))
          (lambda () ($proc) #f)))))

  (define (works? expr) expr #t)

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
        ((_ (raises body ...))
          #`(check (raises? (lambda () body ...))))
        ((_ (works body ...))
          #`(check (works? (lambda () body ...))))
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
