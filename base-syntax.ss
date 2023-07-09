(library (base-syntax)
  (export
    index-switch
    define-data-constructor
    define-data-accessors)

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

  ; --------------------------------------

  (define-syntax define-data-constructor 
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (lets
            ($fields (syntax->list #`(field ...)))
            #`(define-syntax-rule (name field ...)
              #,(case (length $fields)
                ((0) #f)
                ((1) (car $fields))
                ((2) #`(cons #,(car $fields) #,(cadr $fields)))
                (else #`(vector #,@$fields)))))))))

  (define-syntax define-data-accessors
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

)