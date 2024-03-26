(library (labs macro)
  (export
    macro-case
    macro-rules
    define-macro-literal?
    define-macro-matcher
    define-macro
    define-macros
    pattern-rules
    macro-case-2
    macro-rules-2
    define-macro-2
    define-macros-2)

  (import
    (micascheme)
    (labs syntax-match))

  (define-rule-syntax (define-macro-literal? $id)
    (define-literal? $id))

  (define-rule-syntax (define-macro-matcher $id $entry ...)
    (define-syntax-matcher $id $entry ...))

  (define-rule-syntax (macro-case $lookup $syntax $entry ...)
    (syntax-match $lookup $syntax
      (list #'$entry ...)))

  (define-syntax (macro-case-2 $syntax $lookup)
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

  (define-syntax (macro-rules-2 $syntax $lookup)
    (syntax-case $syntax ()
      ((_ ($pattern $body)...)
        #`(lambda ($syntax)
          (macro-case-2 $syntax ($pattern #'$body) ...)))))

  (define-rule-syntax (macro-rules $entry ...)
    (lambda ($syntax)
      (lambda ($lookup)
        (macro-case $lookup $syntax $entry ...))))

  (define-rule-syntax (define-macro $name $entry ...)
    (define-syntax $name
      (macro-rules $entry ...)))

  (define-rule-syntax (define-macro-2 $name $entry ...)
    (define-syntax $name
      (macro-rules-2 $entry ...)))

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

  (define-syntax (define-macros-2 $syntax)
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
                #`(define-macro-2
                  #,(car $group)
                  #,@(cdr $group)))
              $groups))))))

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
  (define-syntax (pattern-rules $syntax)
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
              (syntax->list #'($rule ...))))))))
)
