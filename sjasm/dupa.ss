(library (sjasm dupa)
  (export dupa)
  (import (micascheme) (sjasm split))

  (define-syntax (dupa $syntax)
    (syntax-case $syntax ()
      ((_ arg ...)
        (splita #'(arg ...)))))
)
