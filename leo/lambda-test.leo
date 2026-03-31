import leo scheme

check equal?
  with: lambda 123
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
