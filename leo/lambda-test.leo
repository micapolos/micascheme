import
  except: lambda, case-lambda, from scheme
  leo check
  leo with
  leo lambda

check procedure? lambda 123

check procedure?
  lambda
    with x
    x

check procedure?
  lambda
    with: x, y, z
    z

check procedure?
  lambda
    with: x, y, and z
    z

check equal?
  with lambda 123
  123

check equal?
  with
    lambda: with x, x
    123
  123

check equal?
  with
    lambda
      with: x, y, z
      string-append: x, y, z
    "a", "b", "c"
  "abc"

check equal?
  with
    lambda
      with: x, y, and z
      apply: string-append, x, y, z
    "a", "b"
  "ab"

check equal?
  with
    lambda
      with: x, y, and z
      apply: string-append, x, y, z
    "a", "b", "c", "d", "e", "f"
  "abcdef"
