import leo scheme

check
  equal?
    switch
      123
      when
        number? number
        add: number, 1
      when
        string? string
        string-append: string, "!"
    124

check
  equal?
    switch
      "foo"
      when
        number? number
        add: number, 1
      when
        string? string
        string-append: string, "!"
    "foo!"

check
  void?
    switch
      char A
      when
        number? number
        add: number, 1
      when
        string? string
        string-append: string, "!"
