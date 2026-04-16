todo
  custom read-handler
  custom write-handler
  fix check to raise proper exceptions with: cause, hint
  read non-null terminated lists using trailing dot
  fix
    import, export, rename, and more
    in leo scheme
  make eval work
  line comment using colon
  syntax-rules
  command line flags
    library directories
  figure out how to
    compile separate libraries for
      optimization level 2
      optimication level 3
  implement libraries matching r7rs standard
    leo base
    leo string
    etc ...
  replace
    cause-condition, hint-condition
    with causes-condition
  documentation
  consider this when reading quasiquotes
    every quasiquote must have a closing unquote at every leaf
    examples
      `quoted sentence`
      `circle
        radius` 10
        center point
          x` 20
          y` 30
          z undefined`
    do not read 'quote symbol as quote
    let it be explicit like syntax
    rename
      quasiquote to begin-quote
      unquote to end-quote
      unquote-splicing to end-quote-splicing
