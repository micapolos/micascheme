(library (syntaxes)
  (export
    define-case-syntaxes
    define-rules-syntaxes)
  (import
    (scheme)
    (syntax)
    (list)
    (lets)
    (pair))

  (define-syntax (define-case-syntaxes $syntax)
    (syntax-case $syntax ()
      ((_ ($literal ...) $clause ...)
        #`(begin
          #,@(map
            (lambda ($group)
              (lets
                ((pair $name $clauses) $group)
                #`(define-syntax (#,$name $syntax)
                  (syntax-case $syntax ($literal ...)
                    #,@$clauses))))
            (group-by
              syntax-rule-id
              free-identifier=?
              (syntax->list #'($clause ...))))))))

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax ()
      ((_ ($literal ...) $rule ...)
        #`(begin
          #,@(map
            (lambda ($rules-group)
              (lets
                ((pair $name $rules) $rules-group)
                #`(define-syntax #,$name
                  (syntax-rules ($literal ...)
                    #,@$rules))))
            (group-by
              syntax-rule-id
              free-identifier=?
              (syntax->list #'($rule ...))))))))
)
