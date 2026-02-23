(library (leo2 binding)
  (export
    binding binding? binding-procedure binding-ref
    binding-apply)
  (import
    (leo2 base)
    (leo2 term))

  (define (binding $term $procedure)
    (application $procedure $term))

  (define (binding? $term)
    (switch? $term
      ((application? $application)
        (procedure? (application-lhs $application)))))

  (define (binding-procedure $binding)
    (application-lhs $binding))

  (define (binding-ref $binding)
    (application-rhs $binding))

  (define (binding-apply $binding $arg)
    ((binding-procedure $binding) $arg))
)
