(library (labs macro)
  (export
    define-literal?
    define-matcher
    macro-case
    macro-rules
    define-macro
    define-macros)

  (import
    (micascheme)
    (labs syntax-match))

  (define-rule-syntax (define-literal? $id)
    (define-property $id pattern-literal? #t))

  (define-syntax define-matcher
    (syntax-rules ()
      ((_ $name $pattern-matcher)
        (identifier? #'$name)
        (begin
          (define-aux-keyword $name)
          (define-property $name pattern-matcher $pattern-matcher)))
      ((_ ($name $syntax) $body)
        (identifiers? #'($name $syntax))
        (define-pattern-matcher $name
          (lambda ($syntax)
            $body)))))

  (define-syntax (macro-case $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $syntax $clause ...)
        #`(or
          #,@(map
            (lambda ($clause)
              (syntax-case $clause ()
                (($pattern $body)
                  (parse-pattern-match $lookup #'$syntax #'$pattern #'$body))))
            (syntax->list #'($clause ...)))
          (syntax-error #'$syntax)))))

  (define-syntax (macro-rules $syntax $lookup)
    (syntax-case $syntax ()
      ((_ ($pattern $body)...)
        #`(lambda ($syntax)
          (macro-case $syntax ($pattern #'$body) ...)))))

  (define-rule-syntax (define-macro $name $entry ...)
    (define-syntax $name
      (macro-rules $entry ...)))

  (define-syntax (define-macros $syntax)
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
              $groups))))))
)
