(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntax))

  (define-aux-keyword body)

  (meta define (params $spec)
    (syntax-case $spec (values)
      ($identifier
        (identifier? #'$identifier)
        #'($identifier))
      ((values $identifier ...)
        (for-all identifier? (syntax->list #'($identifier ...)))
        #'($identifier ...))))

  (define-syntax fluent
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax (values)
          ((_ (values $value ...) $item ...)
            (syntax-case #'($item ...) ()
              (()
                #'(values $value ...))
              (($first-item $item ...)
                (syntax-case #'$first-item (define let fluent values lambda apply)
                  ((define $spec $body ...)
                    (let (($tmps (generate-temporaries (syntax->list #'($value ...)))))
                      #`(let-values
                        (
                          ((#,@$tmps) (values $value ...))
                          (#,(params #'$spec) (fluent $body ...)))
                        (fluent (values #,@$tmps) $item ...))))
                  ((let $spec $sub-item ...)
                    #`(fluent
                      (values
                        (let-values
                          ((#,(params #'$spec) (values $value ...)))
                          (fluent $sub-item ...)))
                      $item ...))
                  ((lambda $body ...)
                    #'(fluent
                      (values (lambda ($value ...) (fluent $body ...)))
                      $item ...))
                  ((apply $sub-item ...)
                    #'(fluent
                      (values ($value ... $sub-item ...))
                      $item ...))
                  ((fluent $sub-item ...)
                    #'(fluent
                      (values $value ... (fluent $sub-item ...))
                      $item ...))
                  ((values $sub-item ...)
                    #'(fluent
                      (values $value ... $sub-item ...)
                      $item ...))
                  (($identifier)
                    (identifier? #'$identifier)
                    #'(fluent
                      (values ($identifier $value ...))
                      $item ...))
                  (($identifier $arg ...)
                    (identifier? #'$identifier)
                    #'(fluent
                      (values ($identifier $value ... (fluent $arg ...)))
                      $item ...))
                  ($other
                    #'(fluent
                      (values $value ... $other)
                      $item ...))))))
          ((_ $item ...)
            #'(fluent (values) $item ...))))))
)
