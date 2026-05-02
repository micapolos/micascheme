import
  leo check
  leo eval
  leo let
  leo with
  leo math
  only
    equal?, environment, quasiquote
    from scheme

check equal?
  eval
    'let
      with
        x 10
        y 20
      add: x, y
    environment 'leo scheme
  30
