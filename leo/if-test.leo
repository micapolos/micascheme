import
  except: if, from scheme
  leo check
  leo if

check equal?
  if
    equal?: 2, 2
    "OK"
    "not OK"
  "OK"

check equal?
  if
    equal?: 2, 3
    "OK"
    "not OK"
  "not OK"

check equal?
  if
    equal?: 2, 2
    then
      10
      20
      "OK"
    else
      30
      40
      "not OK"
  "OK"
