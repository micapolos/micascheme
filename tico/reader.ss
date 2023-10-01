(library (tico reader)
  (export read-typed)
  (import
    (micascheme)
    (tico term)
    (tico type)
    (leo reader))

  (data (context))

  (define-syntax-rule (read-typed $item ...)
    (reader-eval
      (context->typed-reader (context) identity)
      $item ...))

  (define (context->typed-reader $context $end-fn)
    (context->args-reader $context
      (lambda ($args)
        ($end-fn
          (or
            (single $args)
            (error `non-single-arg "dupa"))))))

  (define (context->args-reader $context $end-fn)
    (context-arg-stack->args-reader $context (stack) $end-fn))

  (define (context-arg-stack->args-reader $context $arg-stack $end-fn)
    (reader $arg-stack
      ; append-fn
      (lambda ($item)
        (context-arg-stack->args-reader $context
          (push $arg-stack
            (switch $item
              ((boolean? $boolean) (typed $boolean (boolean-type)))
              ((number? $number) (typed $number (number-type)))
              ((string? $string) (typed $string (string-type)))
              ((else $other) (error `append "invalid" $item))))
          $end-fn))
      ; begin-fn
      (lambda ($name)
        (case $name
          (else
            (context->args-reader $context
              (lambda ($args)
                (context-arg-stack->args-reader $context
                  (push $arg-stack
                    (typed
                      (application `list (map typed-value $args))
                      (struct-type $name (map typed-type $args))))
                  $end-fn))))))
      ; end-fn
      (lambda ($args)
        ($end-fn (reverse $args)))))
)
