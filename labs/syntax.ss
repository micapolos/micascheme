(library (labs syntax)
  (export
    syntax-flatten
    syntax-flatten-rec)
  (import (micascheme))

  (define (syntax-flatten $syntax)
    (syntax-case $syntax (begin)
      ((begin $body ...)
        (syntax->list #'($body ...)))
      ($other (list #'$other))))

  (define (syntax-flatten-rec $syntax)
    (syntax-case $syntax (begin)
      ((begin $body ...)
        (flatten (map syntax-flatten-rec (syntax->list #'($body ...)))))
      ($other (list #'$other))))
)
