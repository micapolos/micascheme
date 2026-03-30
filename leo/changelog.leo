import leo scheme

quote changelog
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
