import leo scheme

define first-parameter make-thread-parameter #f
define second-parameter make-thread-parameter #f

check equal?
  parameterize in 123
  123

check equal?
  parameterize
    first-parameter "Hello"
    second-parameter "world"
    in string-append
      with first-parameter
      ", "
      with second-parameter
      "!"
  "Hello, world!"
