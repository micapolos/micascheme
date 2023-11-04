(library (tico dependency)
  (export
    dependency dependency? dependency-symbol dependency-packet
    test-dependency
    dependency-lets-datum
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

  (define (dependency-lets-datum $dependency)
    `(
      ,(dependency-symbol $dependency)
      ,(packet-datum
        (dependency-packet $dependency))))

  (define (tuple-dependencies $dependencies-list)
    (apply append (reverse $dependencies-list)))
)
