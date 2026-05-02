import
  only: equal?, call-with-string-output-port, quasiquote, from scheme
  only: lines-string, from string
  leo check
  leo with
  leo lambda
  leo display

check equal?
  call-with-string-output-port lambda
    with port
    display: `hello world, port
  lines-string "hello world"

check equal?
  call-with-string-output-port lambda
    with port
    display-string: "hello", port
  "hello"
