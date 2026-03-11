map
  append
    prepend
      optional one-of: '+', '-'
      non-empty-list numeric-char
    or
      optional prepend: '.', non-empty-list numeric-char
      null
  ?filter
  list->string
  string->number
