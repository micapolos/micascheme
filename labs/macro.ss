(library (labs macro)
  (export
    macro-rules
    define-macro-literal?
    define-macro-matcher
    define-macro
    define-macros)

  (import
    (micascheme)
    (labs syntax-match))

  (define-syntax-rule (define-macro-literal? $id)
    (define-literal? $id))

  (define-syntax-rule (define-macro-matcher $id $entry ...)
    (define-syntax-matcher $id $entry ...))

  (define-syntax macro-rules
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $entry ...)
          #`(lambda ($syntax)
            (lambda ($lookup)
              (or
                (syntax-match $lookup $syntax
                  (list #'$entry ...))
                (syntax-error $syntax))))))))

  (define-syntax define-macro
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $name $entry ...)
          #`(define-syntax $name
            (macro-rules $entry ...))))))

  (define-syntax define-macros
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $rule ...)
          (lets
            ($groups
              (group-by
                syntax-rule-id
                free-identifier=?
                (syntax->list #'($rule ...))))
            #`(begin
              #,@(map
                (lambda ($group)
                  #`(define-macro
                    #,(car $group)
                    #,@(cdr $group)))
                $groups)))))))
)
