(library (sourced)
  (export sourced)
  (import
    (scheme)
    (data)
    (switch)
    (lets)
    (list)
    (syntax))

  ; returns three values:
  ; - procedure which would return sourced expression
  ; - the sourced expression as datum
  ; - a list of source annotations, potentially empty if no information is available
  (define-syntax (sourced $syntax)
    (syntax-case $syntax ()
      ((_ expr)
        (lets
          ($sources
            (switch? (syntax->annotation #'expr)
              ((annotation? $annotation)
                (call-with-values
                  (lambda ()
                    (locate-source-object-source
                      (annotation-source $annotation) #t #t))
                  (case-lambda
                    (() '())
                    (($path $line $column)
                      `(
                        (path ,$path)
                        (at
                          (line ,$line)
                          (column ,$column)))))))))
          #`(values
            (lambda () expr)
            'expr
            '#,(datum->syntax #'sourced $sources))))))
)
