(library (tico binding)
  (export
    binding binding? binding-switch
    generate-parameter-binding
    binding-match
    bindings-match
    binding-not-empty?
    typing->binding
    bindings-ref
    bindings-get
    bindings-resolve-opt
    bindings-resolve
    bindings-typing-ref
    bindings-typing-get
    bindings-stack-typing)
  (import
    (micascheme)
    (tico typing)
    (tico type)
    (tico typing))

  (enum (binding typing expanding))
  (data (expanding))

  (define (typing->binding $typing)
    (binding (typing-parameter $typing)))

  (define (binding-not-empty? $binding)
    (binding-switch $binding
      ((typing? $typing) (typing-not-empty? $typing))
      ((expanding? _) #f)))

  (define (generate-parameter-binding $type)
    (binding (generate-parameter-typing $type)))

  (define (binding-match $binding $pattern $index)
    (binding-switch $binding
      ((typing? $typing)
        (and
          (type-matches? (typing-type $typing) $pattern)
          (typing-variable $typing $index)))
      ((expanding? $expanding)
        TODO)))

  (define (bindings-match-from $bindings $pattern $index)
    (and (not (null? $bindings))
      (unpair $bindings $binding $bindings
        (or
          (binding-match $binding $pattern $index)
          (bindings-match-from $bindings $pattern
            (+ $index (if (binding-not-empty? $binding) 1 0)))))))

  (define (bindings-match $bindings $pattern)
    (bindings-match-from $bindings $pattern 0))

  (define (bindings-ref $bindings $pattern)
    (or
      (bindings-match $bindings $pattern)
      (throw bindings-ref $bindings $pattern)))

  (define (bindings-get $bindings $patterns)
    (switch $patterns
      ((null? _) (throw empty-patterns))
      ((pair? $pair)
        (unpair $pair $pattern $patterns
          (typing-get
            (bindings-ref $bindings $pattern)
            $patterns)))))

  (define (bindings-resolve-opt $bindings $typings)
    (or
      (bindings-resolve-application-opt $bindings $typings)
      (bindings-resolve-argument-access-opt $bindings $typings)))

  (define (bindings-resolve-application-opt $bindings $typings)
    (lets
      ($target-typing
        (bindings-match $bindings
          (arrow
            (reverse (map typing-type $typings))
            (any-type))))
      (and $target-typing
        (stack
          (typing-application $target-typing
            (reverse $typings))))))

  (define (bindings-resolve-argument-access-opt $bindings $typings)
    (opt-lets
      ($typing (single $typings))
      ($target-typing
        (bindings-match $bindings
          (argument-type
            (typing-type $typing)
            (any-type))))
      (stack
        (typing-argument-access $target-typing $typing))))

  (define (bindings-resolve $bindings $typings)
    (or
      (bindings-resolve-opt $bindings $typings)
      $typings))

  (define (bindings-typing-ref $bindings $typing $pattern)
    (or
      (typing-ref $typing $pattern)
      (opt-lets
        ($property-typing
          (bindings-match $bindings
            (property
              (typing-type $typing)
              (any-type))))
        (typing-access
          $property-typing
          $typing))))

  (define (bindings-typing-get $bindings $typing $patterns)
    (fold-left
      (partial bindings-typing-ref $bindings)
      $typing
      $patterns))

  (define (bindings-stack-typing $bindings)
    (fold-left
      stack-typing-push
      (empty-stack-typing)
      (filter typing?
        (map
          binding-body
          (reverse $bindings)))))
)
