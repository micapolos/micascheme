(library (base-syntax)
  (export
    index-switch
    boolean->datum
    number->datum
    string->datum 
    define-struct

    define-one-of-constructor
    define-one-of-switch
    define-one-of->datum
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
          (struct-syntax #`name (syntax->list #`(field ...)) generate-temporary)))))

  (define-syntax define-one-of-constructor
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          (lets
            ($cases (syntax->list #`(case ...)))
            ($size (length $cases))
            ($tmps (generate-temporaries $cases))
            ($rules
              (map
                (lambda ($selected-index $tmp)
                  #`(
                    (_ 
                      #,@(map-indexed
                        (lambda ($index $case)
                          (if (= $index $selected-index) $tmp #`(not #,$case)))
                        $cases))
                    (cons #,$selected-index #,$tmp)))
                (indices $size)
                $tmps))
            #`(define-syntax name
              (syntax-rules (not case ...) 
                #,@$rules)))))))

  (define-syntax define-one-of-switch
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          "foo"))))

  (define-syntax define-one-of->datum
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name case ...))
          "foo"))))

  (define-syntax-rule (define-one-of name case ...)
    (begin
      (define-one-of-constructor name case ...)
      (define-one-of-switch name case ...)
      (define-one-of->datum name case ...)))


)
