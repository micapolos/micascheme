(library (syntaxes)
  (export
    define-case-syntaxes
    define-rules-syntax
    define-rules-syntaxes)
  (import
    (scheme)
    (syntax)
    (procedure)
    (list)
    (lets)
    (pair)
    (syntax-keywords))
  (export (import (syntax-keywords)))

  (define-syntax (define-case-syntaxes $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords $literal ...) $clause ...)
        #`(begin
          #,@(map
            (lambda ($clauses-group)
              (lets
                ((pair $name $clauses) $clauses-group)
                #`(define-syntax #,$name
                  (lambda ($syntax)
                    (syntax-case $syntax ($literal ...)
                      #,@$clauses)))))
            (group-by
              syntax-clause-id
              free-identifier=?
              (syntax->list #'($clause ...))))))
      ((_ $clause ...)
        #`(define-case-syntaxes (keywords) $clause ...))))

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords $literal ...) $rule ...)
        #`(define-case-syntaxes (keywords $literal ...)
          #,@(map syntax-rule->clause (syntax->list #'($rule ...)))))
      ((_ $rule ...)
        #`(define-rules-syntaxes (keywords) $rule ...))))

  (define-syntax (define-rules-syntax $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords $literal ...) $rule $rule* ...)
        (lets
          ($id (syntax-rule-id #'$rule))
          (run
            (unless
              (for-all (partial free-identifier=? $id)
                (map syntax-rule-id (syntax->list #'($rule* ...))))
              (syntax-error $syntax "multiple ids")))
          #`(define-rules-syntaxes (keywords $literal ...) $rule $rule* ...)))
      ((_ $rule ...)
        #`(define-rules-syntax (keywords) $rule ...))))
)
