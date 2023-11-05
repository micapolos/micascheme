(library (tico dependency)
  (export
    dependency dependency? dependency-symbol dependency-packet
    test-dependency
    test-dependencies
    dependency-lets-datum
    dependencies+
    dependencies-flatten)
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

  (define-syntax-rule (test-dependencies $name ...)
    (stack (test-dependency $name) ...))

  (define (dependency-lets-datum $dependency)
    `(
      ,(dependency-symbol $dependency)
      ,(packet-datum
        (dependency-packet $dependency))))

  (define (dependencies+ $first $second)
    (push-all $first $second))

  (define (dependencies-flatten $dependencies-list)
    (fold-left dependencies+ (stack) $dependencies-list))
)
