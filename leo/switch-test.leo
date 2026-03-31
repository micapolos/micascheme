import
  scheme
  leo check
  leo switch
  void

define: foo-string, "foo"

check
  equal?
    switch
      123
      when
        number number?
        +: number, 1
      when
        string string?
        string-append: string, "!"
    124

check
  equal?
    switch
      "foo"
      when
        number number?
        +: number, 1
      when
        string string?
        string-append: string, "!"
    "foo!"

check
  void?
    switch
      'foo
      when
        number number?
        +: number, 1
      when
        string string?
        string-append: string, "!"

check
  equal?
    switch
      123
      when
        number number?
        +: number, 1
      else x
        box x
    124

check
  equal?
    switch
      "foo"
      when
        number number?
        +: number, 1
      else x
        box x
    box "foo"

check
  equal?
    switch foo-string
      when
        number number?
        +: number, 1
      when
        string string?
        string-append: string, "!"
    "foo!"
