(library (micalang syntax-eval)
  (export default-lookup-syntax-eval)
  (import (micalang base))

  (define (default-lookup-syntax-eval $default $lookup $syntax)
    (syntax-case $syntax (lambda if)
      (id
        (identifier? #'id)
        ($lookup #'id))
      ((lambda id body)
        #'(lambda id body))
      ((lhs rhs)
        (lets
          ($lhs (default-lookup-syntax-eval $default $lookup #'lhs))
          ($rhs (default-lookup-syntax-eval $default $lookup #'rhs))
          (syntax-case $lhs (lambda)
            ((lambda id body)
              (default-lookup-syntax-eval
                $default
                (lambda ($id) (if (free-identifier=? $id #'id) $rhs ($lookup $id)))
                #'body))
            (_
              ($default $default $lookup #`(#,$lhs #,$rhs))))))
      ((if a b c)
        (syntax-case (default-lookup-syntax-eval $default $lookup #'a) ()
          (a
            (if (boolean? (datum a))
              #`(default-lookup-syntax-eval $default $lookup (if (datum a) #'b #'c)))
              #`(if a b c))))
      (b (boolean? (datum b)) #'b)
      (x ($default $default $lookup #'x))))
)
