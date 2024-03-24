(library (labs macro)
  (export
    macro-case
    macro-rules
    define-macro-literal?
    define-macro-matcher
    define-macro
    define-macros
    pattern-rules)

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
              (syntax-match $lookup $syntax (list #'$entry ...))))))))

  (define-syntax macro-case
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $lookup $syntax $entry ...)
          #`(syntax-match $lookup $syntax (list #'$entry ...))))))

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

  ; Example:
  ;
  ; (pattern-rules
  ;   ((id1 param1)
  ;     (pat1 arg1)
  ;     (pat2 arg1)))
  ;   ((id2 param1 param2)
  ;     (pat1 arg1 arg2)
  ;     (pat2 arg1 arg2)
  ;     (pat3 arg1 arg2)))
  (define-syntax pattern-rules
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $rule ...)
          #`(lambda ($lookup $syntax $pattern)
            (syntax-case $pattern ()
              #,@(map
                (lambda ($rule)
                  (syntax-case $rule ()
                    (($decl $body ...)
                      (syntax-case #'$decl ()
                        (($id $param ...)
                          #`($decl
                            (syntax-case $syntax ()
                              #,@(map
                                (lambda ($body)
                                  (syntax-case $body ()
                                    (($key $arg ...)
                                      #`($key
                                        (match
                                          #,@(map
                                            (lambda ($param $arg)
                                              #`(#'#,$param #'#,$arg))
                                            (syntax->list #'($param ...))
                                            (syntax->list #'($arg ...))))))))
                                (syntax->list #'($body ...))))))))))
                (syntax->list #'($rule ...)))))))))
)
