import leo scheme

define first-parameter make-thread-parameter #f
define second-parameter make-thread-parameter #f

check equal?
  parameterize
    first-parameter "Hello"
    second-parameter "world"
    in string-append
      first-parameter:
      ", "
      second-parameter:
      "!"
  "Hello, world!"
