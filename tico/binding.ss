(library (tico binding)
  (export
    binding binding? binding-switch
    binding-match
    bindings-match
    binding-not-empty?
    typing->binding)
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
)
