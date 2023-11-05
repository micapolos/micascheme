(library (tico binding)
  (export
    binding binding? binding-switch
    binding-matches?
    typing->binding)
  (import
    (micascheme)
    (tico typing)
    (tico type))

  (enum (binding typing expanding))
  (data (expanding))

  (define (binding-matches? $binding $pattern)
    (binding-switch $binding
      ((typing? $typing)
        (type-matches? (typing-type) $pattern))
      ((expanding? $expanding)
        TODO)))

  (define (typing->binding $typing)
    (binding (typing-parameter $typing)))
)
