import
  leo check
  leo eval
  leo let
  leo in
  leo math
  only: equal?, environment, quote, from scheme

check equal?
  eval
    'let
      x 10
      y 20
      in add: x, y
    environment 'leo scheme
  30
