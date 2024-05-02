(library (syntaxes)
  (export
    define-rules-syntax
    define-rules-syntaxes
    literals)
  (import
    (scheme)
    (syntax)
    (list)
    (lets)
    (pair))

  (define-aux-keyword literals)

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax (literals ellipsis)
      ((_ (ellipsis $ellipsis) (literals $literal ...) $rule ...)
        #`(begin
          #,@(map
            (lambda ($rules-group)
              (lets
                ((pair $name $rules) $rules-group)
                #`(define-syntax #,$name
                  (with-ellipsis $ellipsis
                    (syntax-rules (literals $literal ...) #,@$rules)))))
            (group-by
              syntax-rule-id
              free-identifier=?
              (syntax->list #'($rule ...))))))
      ((_ (literals $literal ...) $rule ...)
        #`(define-rules-syntaxes (ellipsis #,ellipsis) (literals $literal ...) $rule ...))
      ((_ (ellipsis $ellipsis) $rule ...)
        #`(define-rules-syntaxes (ellipsis $ellipsis) (literals) $rule ...))
      ((_ $rule ...)
        #`(define-rules-syntaxes (literals) $rule ...))))

  (define-syntax (define-rules-syntax $syntax)
    (syntax-case $syntax (ellipsis literals)
      ((_ (ellipsis $ellipsis) (literals $literal ...) $rule ...)
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
            (with-ellipsis $ellipsis
              (syntax-rules ($literal ...) #,@$rules)))))
      ((_ (literals $literal ...) $rule ...)
        #`(define-rules-syntax (ellipsis #,ellipsis) (literals $literal ...) $rule ...))
      ((_ (ellipsis $ellipsis) $rule ...)
        #`(define-rules-syntax (ellipsis $ellipsis) (literals) $rule ...))
      ((_ $rule ...)
        #`(define-rules-syntax (literals) $rule ...))))
)
