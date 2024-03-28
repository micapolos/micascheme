(library (labs slick-syntax)
  (export slick-syntax slick-values)
  (import
    (labs slick-keywords)
    (except (micascheme) is))

  (define (slick-syntax $syntax)
    (lets
      ((pair $definitions $values)
        (slick-definitions-and-values $syntax))
      #`(begin
        #,@(map
          (lambda ($definition)
            (lets
              ((pair $params $args) $definition)
              #`(define-values (#,@$params) (values #,@$args))))
          (reverse $definitions))
        #,@(reverse $values))))

  (define (slick-definitions-and-values $syntax)
    (syntax-case $syntax ()
      (($item ...)
        (fold-left
          push-slick
          (pair (stack) (stack))
          (syntax->list #'($item ...))))))

  (define (slick-values $syntax)
    (lets
      ((pair $definitions $values)
        (slick-definitions-and-values $syntax))
      (cond
        ((null? $definitions) $values)
        (else
          (or
            (opt-lets
              ($value (single $values))
              (stack
                #`(lets
                  #,@(map
                    (lambda ($definition)
                      (lets
                        ((pair $params $args) $definition)
                        #`((values #,@$params) (values #,@$args))))
                    (reverse $definitions))
                  #,$value)))
            (syntax-error $syntax "single value expected"))))))

  (define (push-slick $definitions-and-values $syntax)
    (lets
      ((pair $definitions $values) $definitions-and-values)
      (syntax-case $syntax (the doing is)
        ((is $body ...)
          (pair
            (push
              $definitions
              (pair
                (reverse $values)
                (reverse (slick-values #'($body ...)))))
            (stack)))
        ((the $body ...)
          (pair
            $definitions
            (push-all $values
              (slick-values #'($body ...)))))
        ((doing $body ...)
          (pair
            $definitions
            (stack
              #`(lambda (#,@(reverse $values))
                (values #,@(reverse (slick-values #'($body ...))))))))
        (($id $body ...)
          (identifier? #'$id)
          (pair
            $definitions
            (stack
              #`($id
                #,@(reverse $values)
                #,@(reverse (slick-values #'($body ...)))))))
        ($other
          (pair
            $definitions
            (push $values #'$other))))))
)
