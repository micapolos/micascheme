import
  except
    guard
    from scheme
  leo check
  leo guard
  leo keywords

check equal?
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    in 10
  10

check equal?
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    in "foo"
  "foo"

check equal?
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    in raise 20
  21

check equal?
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    in raise "foo"
  "foo!"

check raises
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    in raise #\a

check equal?
  guard
    when
      n number?
      +: n, 1
    when
      s string?
      string-append: s, "!"
    else x
      `other` x
    in raise #\a
  `other #\a
