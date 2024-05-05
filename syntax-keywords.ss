(library (syntax-keywords)
  (export fenders implicit)
  (import (scheme))

  (define-syntax fenders
    (lambda ($syntax)
      (syntax-case $syntax ()
        (id (identifier? #'id)
        (syntax-error #'id "misplaced keyword")))))

  (define-syntax implicit
    (lambda ($syntax)
      (syntax-case $syntax ()
        (id (identifier? #'id)
        (syntax-error #'id "misplaced keyword")))))
)
