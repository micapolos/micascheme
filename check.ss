(library (check)
  (export
    checking?
    check
    raises?)
  (import (scheme))

  (define checking? (make-thread-parameter #f))

  (define (raises? $proc)
    (call/cc
      (lambda (cont)
        (with-exception-handler
          (lambda (_) (cont #t))
          (lambda () ($proc) #f)))))

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
      (syntax-case stx (not)
        ((_ (not (pred arg ...)))
          (let*
            (
              (args (syntax->list #`(arg ...)))
              (tmps (generate-temporaries #`(arg ...)))
              (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
              (ann (syntax->annotation stx))
              (source (annotation-source ann))
              (ann-string (syntax->location-string stx)))
            #`(parameterize ((checking? #t))
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
              (ann (syntax->annotation stx))
              (source (annotation-source ann))
              (ann-string (syntax->location-string stx)))
            #`(parameterize ((checking? #t))
              (let (#,@let-cases)
                (or
                  (pred #,@tmps)
                  (error `check
                    (format "\n~a   expr: ~s\n  value: ~s\n"
                      #,ann-string
                      (quote (pred arg ...))
                      (list (quote pred) #,@tmps)))))))))))
)
