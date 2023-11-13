(library (tico binding)
  (export
    binding binding? binding-switch
    generate-parameter-binding
    binding-match
    bindings-match
    binding-not-empty?
    typing->binding
    bindings-get*
    bindings-resolve-opt
    bindings-resolve
    bindings-do)
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

  (define (bindings-get $bindings $pattern)
    (bindings-match $bindings $pattern))

  (define (bindings-get* $bindings $patterns)
    (switch $patterns
      ((null? _) (throw empty-patterns))
      ((pair? $pair)
        (unpair $pair $pattern $patterns
          (typing-get
            (bindings-get $bindings $pattern)
            $patterns)))))

  (define (bindings-resolve-opt $bindings $typings)
    (or
      (bindings-resolve-application-opt $bindings $typings)
      (bindings-resolve-access-opt $bindings $typings)))

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

  (define (bindings-resolve-access-opt $bindings $typings)
    (switch-opt $typings
      ((pair? $pair)
        (unpair $pair $typing $typings
          (lets
            ($target-typing
              (bindings-match $bindings
                (property
                  (reverse (map typing-type $typings))
                  (typing-type $typing)))) ; TODO: This must already be (type-type) with a runtime value.
            (and $target-typing
              (stack
                (typing-access $target-typing
                  (reverse $typings)))))))))

  (define (bindings-resolve $bindings $typings)
    (or
      (bindings-resolve-opt $bindings $typings)
      $typings))

  (define (bindings-do $bindings $typings $fn)
    (lets
      ($parameter-typings (ordered-map typing-parameter $typings))
      (typings-do
        $parameter-typings
        $typings
        ($fn
          (push-list $bindings
            (map binding $parameter-typings))))))

  (define (bindings-typing-ref $bindings $typing $pattern)
    (or
      (typing-ref $typing $pattern)
      (and-lets
        ($property-typing
          (bindings-match $bindings
            (property
              (typing-type $typing)
              (any-type))))
        (typing-access $property-typing $pattern))))
)
