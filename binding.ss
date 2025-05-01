(library (binding)
  (export binding-append)
  (import (scheme) (syntax) (identifier))

  (define-syntax (binding-append $syntax)
    (syntax-case $syntax ()
      ((id part ...)
        (apply
          identifier-append
          (syntax id)
          (syntaxes part ...)))))
)
