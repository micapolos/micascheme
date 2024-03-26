(library (syntaxes)
  (export
    define-rules-syntax
    define-rules-syntaxes)
  (import
    (scheme)
    (syntax)
    (list)
    (lets)
    (pair))

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax ()
      ((_ $literals $rule ...)
        #`(begin
          #,@(map
            (lambda ($rules-group)
              (lets
                ((pair $name $rules) $rules-group)
                #`(define-syntax #,$name
                  (syntax-rules $literals #,@$rules))))
            (group-by
              syntax-rule-id
              free-identifier=?
              (syntax->list #'($rule ...))))))))

  (define-syntax (define-rules-syntax $syntax)
    (syntax-case $syntax ()
      ((_ $literals $rule ...)
        (lets
          ((pair $name $rules)
            (or
              (single
                (group-by
                  syntax-rule-id
                  free-identifier=?
                  (syntax->list #'($rule ...))))
              (syntax-error $syntax "multiple ids")))
          #`(define-syntax #,$name
            (syntax-rules $literals #,@$rules))))))
)
