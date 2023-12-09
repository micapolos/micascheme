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
        (syntax-case $syntax (body)
          ((_ (body ($binding ...) $value ...) $item ...)
            (syntax-case #'($item ...) ()
              (()
                #'(let*-values ($binding ...)
                  (values $value ...)))
              (($first-item $item ...)
                #`(fluent
                  #,(syntax-case #'$first-item (let do fluent values lambda)
                    ((let $spec $body ...)
                      (let (($tmps (generate-temporaries (syntax->list #'($value ...)))))
                        #`(body
                          ($binding ...
                            ((#,@$tmps) (values $value ...))
                            (#,(params #'$spec) (fluent $body ...)))
                          #,@$tmps)))
                    ((do $spec $item ...)
                      #`(let-values
                        ((#,(params #'$spec) (values $value ...)))
                        (fluent $item ...)))
                    ((lambda $body ...)
                      #'(body
                        ($binding ...)
                        (lambda ($value ...)
                          (fluent $body ...))))
                    ((fluent $sub-item ...)
                      #'(body
                        ($binding ...)
                        $value ... (fluent $sub-item ...)))
                    ((values $sub-item ...)
                      #'(body
                        ($binding ...)
                        $value ... $sub-item ...))
                    (($identifier $arg ...)
                      (identifier? #'$identifier)
                      #'(body
                        ($binding ...)
                        ($identifier $value ... $arg ...)))
                    ($other
                      #'(body
                        ($binding ...)
                        $value ... $other)))
                  $item ...))))
          ((_ $item ...)
            #'(fluent (body ()) $item ...))))))
)
