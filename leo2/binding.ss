(library (leo2 binding)
  (export
    binding binding? binding-lambda binding-ref

    binding-apply)
  (import
    (leo2 base)
    (leo2 term))

  (define (binding $term $lambda)
    (application $lambda $term))

  (define (binding? $term)
    (switch? $term
      ((application? $application)
        (lambda? (application-lhs $application)))))

  (define (binding-lambda $binding)
    (application-lhs $binding))

  (define (binding-ref $binding)
    (application-rhs $binding))

  (define (binding-apply $binding $arg)
    ((binding-lambda $binding) $arg))
)
