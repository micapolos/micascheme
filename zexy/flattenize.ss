(library (zexy flattenize)
  (export flattenize)
  (import (micascheme))

  (define (flattenize $syntax-list)
    (flatten
      (map
        (lambda ($syntax)
          (syntax-case $syntax (begin)
            ((begin $body ...)
              (flattenize (syntax->list #'($body ...))))
            ($other
              (list #'$other))))
        $syntax-list)))
)
