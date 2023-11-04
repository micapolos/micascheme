(library (tico dependency)
  (export
    dependency dependency? dependency-symbol dependency-packet
    test-dependency
    tuple-dependencies)
  (import
    (micascheme)
    (tico packet))

  (data (dependency symbol packet))

  (define-syntax test-dependency
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $name) (identifier? #'$name)
          #`(dependency
            (quote $name)
            (test-packet $name))))))

  (define (tuple-dependencies $dependencies-list)
    (apply append (reverse $dependencies-list)))
)
