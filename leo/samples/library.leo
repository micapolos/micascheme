library
  leo term
  export
    native
    native?
    native-ref
    application
    application?
    application-lhs
    application-rhs
    abstraction
    abstraction?
    abstraction-procedure
    hello-world
  import
    leo lang
  data native ref
  data application: lhs, rhs
  data abstraction procedure
  define foo "foo"
  define bar "bar"
  define
    exclamate string
    string-append: string, "!"
  define
    comma-separated: string-a, string-b
    string-append: string-a, string-b
  define hello-world
    comma-separated
      "Hello"
      exclamate "world"
