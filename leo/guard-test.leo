import
  except
    guard
    from scheme
  leo check
  leo guard

check equal?
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
    10
  10

check equal?
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
    "foo"
  "foo"

check equal?
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
    raise 20
  21

check equal?
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
    raise "foo"
  "foo!"

check raises
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
    raise #\a

check equal?
  guard
    x
      when
        number? x
        +: x, 1
      when
        string? x
        string-append: x, "!"
      else
        `other` x
    raise #\a
  'other #\a
