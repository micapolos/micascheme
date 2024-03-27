(library (labs macro)
  (export
    define-syntax-literal?
    define-syntax-matcher
    macro-case-opt
    macro-rules-opt
    macro-case
    macro-rules
    define-macro
    define-macros

    rules-matcher
    define-rules-matcher
    define-matchers)

  (import
    (micascheme)
    (labs pattern-match))

  (define-rule-syntax (define-syntax-literal? $id)
    (define-property $id syntax-literal? #t))

  (define-syntax define-syntax-matcher
    (syntax-rules ()
      ((_ $name $syntax-matcher)
        (identifier? #'$name)
        (define-property $name syntax-matcher $syntax-matcher))
      ((_ ($name $syntax) $body)
        (identifiers? #'($name $syntax))
        (define-syntax-matcher $name
          (lambda ($syntax)
            $body)))))

  (define-syntax (macro-case-opt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $expr $clause ...)
        #`(lets ($syntax $expr)
          #,(parse-pattern-clauses $lookup #'$syntax
            (syntax->list #'($clause ...)))))))

  (define-rule-syntax (macro-case $syntax $clause ...)
    (macro-case-opt $syntax
      $clause ...
      (_ (syntax-error $syntax))))

  (define-syntax (macro-rules-opt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ ($pattern $body ...) ...)
        #`(lambda ($syntax)
          (macro-case-opt $syntax ($pattern #'(begin $body ...)) ...)))))

  (define-syntax (macro-rules $syntax $lookup)
    (syntax-case $syntax ()
      ((_ ($pattern $body ...) ...)
        #`(lambda ($syntax)
          (macro-case $syntax ($pattern #'(begin $body ...)) ...)))))

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

  (define-syntax (rules-matcher $syntax)
    (syntax-case $syntax ()
      ((_ $pattern-rule ...)
        #`(lambda ($pattern)
          (syntax-case $pattern ()
            #,@(map
              (lambda ($pattern-rule)
                (syntax-case $pattern-rule ()
                  ((($id $param ...) $matcher-rule ...)
                    #`(($id $param ...)
                      (values
                        (list #'$param ...)
                        #'(lambda ($stx)
                          (macro-case-opt $stx
                            #,@(map
                              (lambda ($matcher-rule)
                                (syntax-case $matcher-rule ()
                                  (($pattern $arg ...)
                                    #`($pattern (list #'$arg ...)))))
                              (syntax->list #'($matcher-rule ...))))))))))
              (syntax->list #'($pattern-rule ...))))))))

  (define-rule-syntax (define-rules-matcher $name $rule ...)
    (define-syntax-matcher $name
      (rules-matcher $rule ...)))

  (define-syntax (define-matchers $syntax)
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
                #`(define-rules-matcher #,(car $group)
                  #,@(cdr $group)))
              $groups))))))
)
