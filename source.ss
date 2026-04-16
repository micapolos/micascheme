(library (source)
  (export source)
  (import
    (scheme)
    (switch)
    (lets)
    (syntax))

  (define-syntax (source $syntax)
    (syntax-case $syntax ()
      ((_ x)
        (switch (syntax->annotation #'x)
          ((annotation? $annotation)
            (lets
              ((values $path $line $column)
                (locate-source-object-source (annotation-source $annotation) #t #t))
              #`'(source
                x
                (in #,(literal->syntax $path))
                (at
                  (line #,(literal->syntax $line))
                  (column #,(literal->syntax $column))))))
          ((else _) #`'#,$syntax)))))
)
