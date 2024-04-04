(library (labs slick)
  (export slick)
  (import
    (scheme)
    (labs slick-syntax)
    (labs slick-keywords))
  (export
    (import (labs slick-keywords)))

  (define-syntax (slick $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (slick-syntax #'($item ...)))))
)
