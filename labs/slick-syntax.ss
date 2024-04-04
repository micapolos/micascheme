(library (labs slick-syntax)
  (export slick-syntax slick-values)
  (import
    (micascheme))

  (define (slick-syntax $syntax)
    (lets
      ($values (slick-values $syntax))
      #`(values #,@(reverse $values))))

  (define (slick-values $syntax)
    (syntax-case $syntax ()
      (($item ...)
        (fold-left
          push-slick
          (stack)
          (syntax->list #'($item ...))))))

  (define (push-slick $values $syntax)
    (syntax-case $syntax (values)
      ((values $body ...)
        (push-all $values
          (slick-values #'($body ...))))
      (($id $body ...)
        (identifier? #'$id)
        (stack
          #`($id
            #,@(reverse $values)
            #,@(reverse (slick-values #'($body ...))))))
      ($other
        (push $values #'$other))))
)
