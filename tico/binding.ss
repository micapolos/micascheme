(library (tico binding)
  (export
    binding binding? binding-switch
    generate-parameter-binding
    binding-match
    bindings-match
    binding-not-empty?
    typing->binding
    bindings-get*)
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

  (define (bindings-get $bindings $selector-typing)
    (bindings-match $bindings
      (typing->type $selector-typing)))

  (define (bindings-get* $bindings $selector-typings)
    (switch (reverse $selector-typings)
      ((null? _) (stack))
      ((pair? $pair)
        (unpair $pair $selector-typing $selector-typings
          (stack
            (fold-left
              typing-get
              (bindings-get $bindings $selector-typing)
              $selector-typings))))))

)
