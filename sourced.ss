(library (sourced)
  (export sourced)
  (import
    (scheme)
    (data)
    (switch)
    (lets)
    (syntax))

  (define-syntax (sourced $syntax)
    (syntax-case $syntax ()
      ((_ x)
        (lets
          ((values $path $line $column)
            (switch (syntax->annotation #'x)
              ((annotation? $annotation)
                (locate-source-object-source (annotation-source $annotation) #t #t))
              ((else _)
                (values #f #f #f))))
          #`(values x 'x
            #,(literal->syntax $path)
            #,(literal->syntax $line)
            #,(literal->syntax $column))))))
)
