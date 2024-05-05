(library (syntaxes)
  (export
    define-rules-syntax
    define-rules-syntaxes
    literals)
  (import
    (scheme)
    (syntax)
    (procedure)
    (list)
    (lets)
    (pair))

  (define-aux-keyword literals)

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $rule ...)
        #`(begin
          #,@(map
            (lambda ($rules-group)
              (lets
                ((pair $name $rules) $rules-group)
                #`(define-syntax #,$name
                  (lambda ($syntax)
                    (syntax-case $syntax ($literal ...)
                      #,@(map syntax-rule->clause $rules))))))
            (group-by
              syntax-rule-id
              free-identifier=?
              (syntax->list #'($rule ...))))))
      ((_ $rule ...)
        #`(define-rules-syntaxes (literals) $rule ...))))

  (define-syntax (define-rules-syntax $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $rule ...)
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
            (lambda ($syntax)
              (syntax-case $syntax ($literal ...)
                #,@(map syntax-rule->clause $rules))))))
      ((_ $rule ...)
        #`(define-rules-syntax (literals) $rule ...))))
)
