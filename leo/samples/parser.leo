map
  apply append
    apply prepend
      optional one-of: '+', '-'
      non-empty-list numeric-char
    map
      optional apply prepend: '.', non-empty-list numeric-char
      default null
  ?filter
  list->string
  string->number
