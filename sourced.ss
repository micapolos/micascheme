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
        (switch (syntax->annotation #'x)
          ((annotation? $annotation)
            (lets
              ((values $path $line $column)
                (locate-source-object-source (annotation-source $annotation) #t #t))
              #`(values
                x
                'x
                #,(literal->syntax $path)
                #,(literal->syntax $line)
                #,(literal->syntax $column))))
          ((else _)
            #`(values
                x
                'x
                #,(literal->syntax #f)
                #,(literal->syntax #f)
                #,(literal->syntax #f)))))))
)
