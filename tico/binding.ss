(library (tico binding)
  (export
    binding binding? binding-switch
    binding-matches?)
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
)
