(library (syntax-keywords)
  (export fenders implicit keywords)
  (import (scheme))

  (define-syntax keywords
    (lambda ($syntax)
      (syntax-case $syntax ()
        (id (identifier? #'id)
        (syntax-error #'id "misplaced")))))

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
