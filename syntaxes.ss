(library (syntaxes)
  (export
    with-ellipsis
    define-rules-syntax
    define-rules-syntaxes
    literals)
  (import
    (scheme)
    (syntax)
    (transformer)
    (procedure)
    (list)
    (lets)
    (pair))

  (define-aux-keyword literals)

  (define-syntax (with-ellipsis $syntax)
    (syntax-case $syntax ()
      ((_ $ellipsis $body ...)
        (identifier? #'$ellipsis)
        #`(begin
          #,@(map
            (partial replace-identifiers #'$ellipsis ellipsis)
            (syntax->list #'($body ...)))))))

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $rule ...)
        #`(begin
          #,@(map
            (lambda ($rules-group)
              (lets
                ((pair $name $rules) $rules-group)
                #`(define-syntax #,$name
                  (syntax-rules (literals $literal ...) #,@$rules))))
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
            (syntax-rules ($literal ...) #,@$rules))))
      ((_ $rule ...)
        #`(define-rules-syntax (literals) $rule ...))))
)
