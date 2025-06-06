(library (leo state)
  (export
    eval-state
    lookup-state
    end-state
    is-state
    state-push
    state-begin
    state-end
    syntax-binding syntax-binding? syntax-binding-ref
    value-binding value-binding? value-binding-ref)
  (import (micascheme) (syntax lookup))

  (data (state push-proc begin-proc end-proc))

  (data (syntax-binding ref))
  (data (value-binding ref))

  (define (lookup-state $lookup $end-proc)
    (eval-state $lookup (stack) $end-proc))

  (define (state-push $state $value)
    ((state-push-proc $state) $value))

  (define (state-begin $state $identifier)
    ((state-begin-proc $state) $identifier))

  (define (state-end $state)
    ((state-end-proc $state)))

  (define (eval-state $lookup $args $end-proc)
    (state
      (lambda ($atom)
        (eval-state
          $lookup
          (push $args (syntax->datum $atom))
          $end-proc))
      (lambda ($identifier)
        (switch-exhaustive (lookup-ref $lookup $identifier)
          ((syntax-binding? $syntax-binding)
            ((syntax-binding-ref $syntax-binding) $lookup $args $end-proc))
          ((value-binding? $value-binding)
            (eval-state $lookup (stack)
              (lambda ($end-args)
                (eval-state
                  $lookup
                  (reverse
                    (call-with-values
                      (lambda ()
                        (apply
                          (value-binding-ref $value-binding)
                          (append (reverse $args) (reverse $end-args))))
                      list))
                  $end-proc))))))
      (lambda ()
        ($end-proc $args))))

  (define (is-state $lookup $identifier $end-proc)
    (eval-state $lookup (stack)
      (lambda ($args)
        (case (length $args)
          ((1)
            (eval-state
              (lookup+ $lookup $identifier
                (value-binding (car $args)))
              (stack)
              $end-proc))
          (else
            (syntax-error #'is "multiple values"))))))

  (define (end-state $value $end-proc)
    (state
      (lambda ($atom)
        (syntax-error $atom))
      (lambda ($identifier)
        (syntax-error $identifier))
      (lambda ()
        ($end-proc $value))))
)
