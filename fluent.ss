(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntax))

  (define-aux-keyword body)

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
                            (
                              #,(syntax-case #'$spec (values)
                                ($identifier
                                  (identifier? #'$identifier)
                                  #'($identifier))
                                ((values $identifier ...)
                                  (for-all identifier? (syntax->list #'($identifier ...)))
                                  #'($identifier ...)))
                              (fluent $body ...)))
                          #,@$tmps)))
                    ((do (values $identifier ...) $item ...)
                      (for-all identifier? (syntax->list #'($identifier ...)))
                      #'(body
                        ($binding ...)
                        ((lambda ($identifier ...) (fluent $item ...)) $value ...)))
                    ((do $identifier $item ...)
                      (identifier? #'$identifier)
                      #'(body
                        ($binding ...)
                        ((lambda ($identifier) (fluent $item ...)) $value ...)))
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
