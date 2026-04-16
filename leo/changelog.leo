import leo scheme

quote changelog
  v0.1.103
    new form: sourced
  v0.1.102
    write non-null list terminator using trailing dot
    new forms: keywords?, free-keywords?, with-syntax*, with-identifier*
  v0.1.100
    improved forms: let, with-syntax, with-identifier
  v0.1.99
    syntax-case?
  v0.1.98
    add forms: with-syntax, with-implicit
  v0.1.97
    new forms: error
    optimize core forms for simple usage: case, if, syntax-case
  v0.1.96
    leo base library
  v0.1.95
    fix writer for record with no fields
  v0.1.94
    switch? form
  v0.1.93
    data make constructor
  v0.1.92
    add printing syntax
  v0.1.91
    adds: assert, assertion-violation
  v0.1.90
    use colon instead of written list
  v0.1.89
    guard
  v0.1.88
    identifier-syntax
  v0.1.87
    define-syntax: rules, case
  v0.1.86
    fixnum functions: fx-1/wraparound, fx+1/wraparound
  v0.1.85
    with implicit
  v0.1.84
    add missing set!
  v0.1.83
    differentiate between
      write which is complement to read
      print designed to produce readable sentences
  v0.1.82
    implements free-syntax=?
  v0.1.81
    fixes free-keyword?
  v0.1.80
    implement define-property
  v0.1.79
    add support for: partial, keywords, char, eof
  v0.1.78
    write ftype-pointer as sexpr
  v0.1.77
    implement escaped char reader
  v0.1.76
    implement cond
  v0.1.75
    fix problem with missing source positions in syntax errors
  v0.1.74
    implement family of ftype-set! macros
  v0.1.73
    implement: define-ftype, ftype-ref, ftype*-ref
  v0.1.72
    do not optimize and strip debug information
    read parenthesised lines
  v0.1.71
    fixes define-syntax
    fixes colon parser
  v0.1.70
    implement case form
  v0.1.69
    zero argument define with
  v0.1.68
    map define-record to chezscheme syntax
  v0.1.67
    remove experimental grammar rules
      define lambda
      define syntax
      define record
    use plain scheme syntax
      define
      define-syntax
      define-record
  v0.1.66
    implement define record
  v0.1.65
    implement: predicate, is?, definer
  v0.1.64
    implement logging
    implement property support
      maker, getter, setter!
      make, get, set!, ref
  v0.1.63
    implement: todo, comment
  v0.1.62
    implement
      define record
      define record type
      define union
  v0.1.61
    shrink release size
  v0.1.60
    fix a bug in repl which exited after evaluating void
  v0.1.59
    remove display-line
    rewrite display to use write with special string and char handling
    add display-string
  v0.1.58
    fix parser to handle newline correctly
    fix repl to commit after newline if possible
  v0.1.57
    improve core forms: define, lambda, if, cond, switch
  v0.1.56
    run repl only with no command line args
  v0.1.55
    make leo boot file to improve start time
  v0.1.54
    build leo with
      optimize-level 3
      no inspector information
  v0.1.53
    add startup-reference-time-ms flag to measure startup time
    bundle repl in release
    do not bundle micascheme in release
  v0.1.52
    add missing library ansi-string
  v0.1.51
    implements: eval, repl
  v0.1.50
    unified define forms
  v0.1.49
    unified let forms
  v0.1.48
    fix exception printing in repl
  v0.1.47
    fix unnecesary newline in repl
  v0.1.46
    implement multi-line repl
    make reader accept parentheses
  v0.1.45
    implement basic repl using single-line reader
  v0.1.44
    implements single-line reader to be used in repl
  v0.1.43
    implements testing forms: test, check, raises, works
    adds first batch of leo unit tests
  v0.1.42
    implement load function
    fix: let-values, let*-values
  v0.1.41
    implement forms: library, import, export, top-level-program
  v0.1.40
    adds optimize-level command line flag
    loads source files as programs which improves optimizations
  v0.1.39
    change variadic notation to: first, second, and others
  v0.1.38
    writer improvements
      remove hash prefixed primitives
      write last item in improper list as 'and item
  v0.1.37
    quotified reader
  v0.1.36
    quotified writer
  v0.1.35
    improve: write, write-line
  v0.1.34
    improve writing quotes
    improved readme
  v0.1.33
    adds parameters
      code-single-line?
      code-pretty?
      code-line-limit
  v0.1.32
    improved
      write prints indented with hashed atoms
      pretty-print prints indented with pretty atoms
  v0.1.31
    fixed character range in literal parser
  v0.1.30
    adds simple leo document library for writing documents with code snippets
  v0.1.29
    adds example macro to write program and its result
  v0.1.29
    adds example macro to write program and its result
  v0.1.28
    adds readme.leo file which is also a valid leo program
  v0.1.27
    improve printing
      write prints single line with parentheses
      pretty-print prints indented code without parentheses
  v0.1.26
    improve exception messages
  v0.1.25
    prints readable read errors
    adds syntax logging
    drops: then, else, in if
  v0.1.24
    prints exceptions using leo syntax
  v0.1.23
    adds license
  v0.1.22
    adds this changelog
  v0.1.19
    fixes leo executable because of mysterious lambda arity mismatch
  v0.1.18
    supports all forms in: import, export
  v0.1.15
    implements variable arity in: define, lambda
  v0.1.12
    adds support for: define-syntax, syntax-case, define-macro
    implements basic command line arguments in leo executable
      including: version, help
