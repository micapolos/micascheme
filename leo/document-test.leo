import
  leo scheme
  leo document
  only: lines-string, from micascheme

check equal?
  example add: 2, 2
  'example
    program add: 2, 2
    result 4

check equal?
  call-with-string-output-port
    lambda
      with port
      parameterize
        current-output-port port
        in document hello
  lines-string "hello"
