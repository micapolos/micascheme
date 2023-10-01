(library (tico reader)
  (export read-compiled)
  (import
    (micascheme)
    (tico term)
    (tico type)
    (tico compiled)
    (leo reader))

  (data (context depth bindings))

  (define (empty-context)
    (context 0 (stack)))

  (define-syntax-rule (read-compiled $item ...)
    (reader-eval
      (context->compiled-reader (empty-context) identity)
      $item ...))

  (define (context->compiled-reader $context $end-fn)
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
              ((boolean? $boolean) (compiled-boolean $boolean))
              ((number? $number) (compiled-number $number))
              ((string? $string) (compiled-string $string))
              ((else $other) (error `append "invalid" $item))))
          $end-fn))
      ; begin-fn
      (lambda ($name)
        (case $name
          ((take)
            (context->args-reader $context
              (lambda ($args)
                (context-arg-stack->args-reader $context
                  (push-list $arg-stack $args)
                  $end-fn))))
          ((do)
            (context->args-reader $context
              (lambda ($args)
                (context-arg-stack->args-reader $context
                  (reverse $args)
                  $end-fn))))
          (else
            (context->args-reader $context
              (lambda ($args)
                (context-arg-stack->args-reader $context
                  (push $arg-stack (compiled-struct $name $args))
                  $end-fn))))))
      ; end-fn
      (lambda ($args)
        ($end-fn (reverse $args)))))
)
