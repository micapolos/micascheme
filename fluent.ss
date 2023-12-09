(library (fluent)
  (export fluent as in)
  (import
    (scheme)
    (syntax))

  (define-aux-keyword as)
  (define-aux-keyword in)

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
            (syntax-case #'($item ...) (as in)
              (()
                #'(values $value ...))
              (((as $param ...) (in $body ...) $item ...)
                #'(fluent
                  (values
                    (let-values
                      ((($param ...) (values $value ...)))
                      (fluent $body ...)))
                  $item ...))
              (($first-item $item ...)
                (syntax-case #'$first-item (define fluent values lambda apply)
                  ((define $spec $body ...)
                    (let (($tmps (generate-temporaries (syntax->list #'($value ...)))))
                      #`(let-values
                        (
                          ((#,@$tmps) (values $value ...))
                          (#,(params #'$spec) (fluent $body ...)))
                        (fluent (values #,@$tmps) $item ...))))
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
                  (($identifier (values $arg ...))
                    (identifier? #'$identifier)
                    #'(fluent
                      (values ($identifier $value ... $arg ...))
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
