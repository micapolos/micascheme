library
  leo examples library
  export
    hello
    leo
    exclamate
    comma-separated
    hello-leo
  import
    leo scheme

  define hello "Hello"
  define leo "Leo"
  define exclamation "!"

  define lambda
    exclamate string
    string-append: string, exclamation

  define lambda
    comma-separated: string-a, string-b
    string-append: string-a, string-b

  define hello-leo
    comma-separated: hello, exclamate leo
