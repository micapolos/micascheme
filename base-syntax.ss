(library (base-syntax)
  (export
    index-switch
    
    boolean->datum
    number->datum
    string->datum

    define-struct
    define-one-of)

  (import (chezscheme) (base))

  (define-syntax index-switch
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr branch ... default)
          #`(case expr
            #,@(map-indexed
              (lambda ($index $branch) #`((#,$index) #,$branch))
              (syntax->list #`(branch ...)))
            (else default))))))

  (define-syntax-rule (boolean->datum $boolean) $boolean)
  (define-syntax-rule (number->datum $number) $number)
  (define-syntax-rule (string->datum $string) $string)

  (define-syntax define-struct
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (struct-syntax 
            #`name 
            (syntax->list #`(field ...)))))))

  (define-syntax define-one-of-constructor
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          (one-of-constructor-syntax 
            #`name 
            (syntax->list #`(case ...)))))))

  (define-syntax define-one-of-switch
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          (one-of-switch-syntax 
            #`name
            (syntax->list #`(case ...)))))))

  (define-syntax define-one-of->datum
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          (one-of->datum-syntax 
            #`name
            (syntax->list #`(case ...)))))))

  (define-syntax define-one-of
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          (one-of-syntax 
            #`name
            (syntax->list #`(case ...)))))))
)
