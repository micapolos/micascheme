import leo scheme

check equal?
  eval
    'let
      x 10
      y 20
      in add: x, y
    environment 'leo scheme
  30
