(library (syntaxes)
  (export
    define-case-syntaxes
    define-rules-syntax
    define-rules-syntaxes
    literals)
  (import
    (scheme)
    (syntax)
    (procedure)
    (list)
    (lets)
    (pair)
    (syntax-keywords))
  (export (import (syntax-keywords)))

  (define-aux-keyword literals)

  (define-syntax (define-case-syntaxes $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $clause ...)
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
        #`(define-case-syntaxes (literals) $clause ...))))

  (define-syntax (define-rules-syntaxes $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $rule ...)
        #`(define-case-syntaxes (literals $literal ...)
          #,@(map syntax-rule->clause (syntax->list #'($rule ...)))))
      ((_ $rule ...)
        #`(define-rules-syntaxes (literals) $rule ...))))

  (define-syntax (define-rules-syntax $syntax)
    (syntax-case $syntax (literals)
      ((_ (literals $literal ...) $rule $rule* ...)
        (lets
          ($id (syntax-rule-id #'$rule))
          (run
            (unless
              (for-all (partial free-identifier=? $id)
                (map syntax-rule-id (syntax->list #'($rule* ...))))
              (syntax-error $syntax "multiple ids")))
          #`(define-rules-syntaxes (literals $literal ...) $rule $rule* ...)))
      ((_ $rule ...)
        #`(define-rules-syntax (literals) $rule ...))))
)
