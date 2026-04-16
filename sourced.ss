(library (sourced)
  (export sourced)
  (import
    (scheme)
    (data)
    (switch)
    (lets)
    (list)
    (syntax))

  (define-syntax (sourced $syntax)
    (syntax-case $syntax ()
      ((_ x)
        (lets
          ((values $path $line $column)
            (switch (syntax->annotation #'x)
              ((annotation? $annotation)
                (switch
                  (values->list
                    (locate-source-object-source
                      (annotation-source $annotation) #t #t))
                  ((null? _) (values #f #f #f))
                  ((else $list) (apply values $list))))
              ((else _)
                (values #f #f #f))))
          #`(values x 'x
            #,(literal->syntax $path)
            #,(literal->syntax $line)
            #,(literal->syntax $column))))))
)
