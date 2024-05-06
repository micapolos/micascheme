(library (syntax-keywords)
  (export fenders implicit)
  (import (scheme))

  (define-syntax fenders
    (lambda ($syntax)
      (syntax-case $syntax ()
        (id (identifier? #'id)
        (syntax-error #'id "misplaced")))))

  (define-syntax implicit
    (lambda ($syntax)
      (syntax-case $syntax ()
        (id (identifier? #'id)
        (syntax-error #'id "misplaced")))))
)
