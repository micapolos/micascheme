(library (labs slick)
  (export slick)
  (import
    (scheme)
    (labs slick-syntax))
  (export (import (labs slick-syntax)) then)

  (define-syntax (slick $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (slick-syntax #'($item ...)))))
)
