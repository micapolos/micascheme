(library (labs slick)
  (export slick)
  (import
    (scheme)
    (labs slick-syntax))

  (define-syntax (slick $syntax)
    (syntax-case $syntax ()
      ((_ $item ...)
        (slick-syntax #'($item ...)))))
)
