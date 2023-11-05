(library (tico binding)
  (export
    binding binding? binding-body
    binding-switch
    binding-matches?)
  (import
    (micascheme)
    (tico typing)
    (tico type))

  (data (binding body))
  (data (expanding))

  (define-syntax-rule (binding-switch $binding $case ...)
    (switch (binding-body $binding) $case ...))

  (define (binding-matches? $binding $pattern)
    (binding-switch $binding
      ((typing? $typing)
        (type-matches? (typing-type) $pattern))
      ((expanding? $expanding)
        TODO)))
)
