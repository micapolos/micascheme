(library (tico reader)
  (export read-typed)
  (import
    (micascheme)
    (tico term)
    (tico type)
    (leo reader))

  (data (context depth bindings))

  (define (empty-context)
    (context 0 (stack)))

  (define-syntax-rule (read-typed $item ...)
    (reader-eval
      (context->typed-reader (empty-context) identity)
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
                  (push $arg-stack
                    (typed
                      (application `list (map typed-value $args))
                      (struct-type $name (map typed-type $args))))
                  $end-fn))))))
      ; end-fn
      (lambda ($args)
        ($end-fn (reverse $args)))))

  (define (type-reader $end-fn)
    (type-stack-reader
      (lambda ($type-stack)
        ($end-fn
          (or
            (single $type-stack)
            (error `non-single-type "dupa"))))))

  (define (type-stack-reader $end-fn)
    (type-stack->push-reader (stack) $end-fn))

  (define (type-stack->push-reader $type-stack $end-fn)
    (reader
      $type-stack
      (lambda ($datum)
        (error `append "dupa"))
      (lambda ($symbol)
        (case $symbol
          ((boolean)
            (type-stack-reader
              (lambda (_)
                (type-stack->push-reader
                  (push $type-stack (boolean-type))
                  $end-fn))))
          ((number)
            (type-stack-reader
              (lambda (_)
                (type-stack->push-reader
                  (push $type-stack (number-type))
                  $end-fn))))
          ((string)
            (type-stack-reader
              (lambda (_)
                (type-stack->push-reader
                  (push $type-stack (string-type))
                  $end-fn))))))
      $end-fn))
)
