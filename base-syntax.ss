(library (base-syntax)
  (export
    index-switch
    test
    
    boolean->datum
    number->datum
    string->datum)

  (import
    (scheme)
    (base)
    (define-syntax))

  (define-syntax index-switch
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr branch ... default)
          #`(case expr
            #,@(map-indexed
              (lambda ($index $branch) #`((#,$index) #,$branch))
              (syntax->list #`(branch ...)))
            (else default))))))

  (define-syntax test
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $spec ...)
          #`(begin
            #,@(map
              (lambda ($spec)
                #`(let ()
                  (load
                    #,(string-append
                      (apply string-append
                        (intercalate
                          (map symbol->string (syntax->datum $spec))
                          "/"))
                      "-test.ss"))))
                (syntax->list #'($spec ...))))))))

  (define-syntax-rule (boolean->datum $boolean) $boolean)
  (define-syntax-rule (number->datum $number) $number)
  (define-syntax-rule (string->datum $string) $string)
)
