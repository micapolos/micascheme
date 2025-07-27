(library (monoid)
  (export monoid-empty monoid-append)
  (import (scheme) (identifier))

  (define-syntax (monoid-empty $syntax)
    (syntax-case $syntax ()
      ((_ id)
        #`(#,(identifier-append #'id #'empty #'- #'id)))))

  (define-syntax (monoid-append $syntax)
    (syntax-case $syntax ()
      ((_ id x ...)
        #`(#,(identifier-append #'id #'id #'- #'append) x ...))))
)
