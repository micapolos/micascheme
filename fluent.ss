(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntax))

  (define-syntax fluent
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax (values)
          ((_ (values $value ...) $item ...)
            (syntax-case #'($item ...) ()
              (()
                #'(values $value ...))
              (($first-item $item ...)
                #`(fluent
                  #,(syntax-case #'$first-item (do fluent values lambda)
                    ((do (values $identifier ...) $item ...)
                      (for-all identifier? (syntax->list #'($identifier ...)))
                      #'(values
                        ((lambda ($identifier ...) (fluent $item ...)) $value ...)))
                    ((do $identifier $item ...)
                      (identifier? #'$identifier)
                      #'(values
                        ((lambda ($identifier) (fluent $item ...)) $value ...)))
                    ((lambda $body ...)
                      #'(values
                        (lambda ($value ...)
                          (fluent $body ...))))
                    ((fluent $sub-item ...)
                      #'(values $value ... (fluent $sub-item ...)))
                    ((values $sub-item ...)
                      #'(values $value ... $sub-item ...))
                    (($identifier $arg ...)
                      (identifier? #'$identifier)
                      #'(values ($identifier $value ... $arg ...)))
                    ($other
                      #'(values $value ... $other)))
                  $item ...))))
          ((_ $item ...)
            #'(fluent (values) $item ...))))))
)
