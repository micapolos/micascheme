(library (base-syntax)
  (export
    index-switch
    define-struct-constructor
    define-struct-accessors
    boolean->datum
    number->datum
    string->datum 
    define-struct->datum
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

  (define-syntax define-struct-constructor 
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (struct-constructor-syntax #`name (syntax->list #`(field ...)))))))

  (define-syntax define-struct-accessors
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (lets
            ($name-string (symbol->string (syntax->datum #`name)))
            ($fields (syntax->list #`(field ...)))
            ($field-strings (map symbol->string (map syntax->datum $fields)))
            ($accessor-strings (map (lambda ($field-string) (string-append $name-string "-" $field-string)) $field-strings))
            ($accessors (map (partial datum->syntax #`name) (map string->symbol $accessor-strings)))
            (case (length $accessors)
              ((0) #`(begin))
              ((1) #`(define-syntax-rule (#,(car $accessors) expr) expr))
              ((2) 
                #`(begin
                  (define-syntax-rule (#,(car $accessors) expr) (car expr))
                  (define-syntax-rule (#,(cadr $accessors) expr) (cdr expr))))
              (else 
                #`(begin
                  #,@(map-indexed 
                    (lambda ($index $accessor)
                      #`(define-syntax-rule (#,$accessor expr) (vector-ref expr #,$index)))
                    $accessors)))))))))

  (define-syntax-rule (boolean->datum $boolean) $boolean)
  (define-syntax-rule (number->datum $number) $number)
  (define-syntax-rule (string->datum $string) $string)

  (define-syntax define-struct->datum 
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (lets
            ($name-string (symbol->string (syntax->datum #`name)))
            ($name->datum-string (string-append $name-string "->datum"))
            ($name->datum (datum->syntax #`name (string->symbol $name->datum-string)))
            ($fields (syntax->list #`(field ...)))
            ($field-strings (map symbol->string (map syntax->datum $fields)))
            ($accessor-strings (map (lambda ($field-string) (string-append $name-string "-" $field-string)) $field-strings))
            ($accessors (map (partial datum->syntax #`name) (map string->symbol $accessor-strings)))
            ($datum-strings (map (lambda ($field-string) (string-append $field-string "->datum")) $field-strings))
            ($datums (map (partial datum->syntax #`name) (map string->symbol $datum-strings)))
            ($tmp (car (generate-temporaries `(tmp))))
            #`(define (#,$name->datum #,$tmp)
              (quasiquote
                (name
                  #,@(map 
                    (lambda ($accessor $datum) #`(unquote (#,$datum (#,$accessor #,$tmp))))
                    $accessors
                    $datums)))))))))

  (define-syntax-rule (define-struct name field ...)
    (begin
      (define-struct-constructor name field ...)
      (define-struct-accessors name field ...)
      (define-struct->datum name field ...)))

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
