import
  leo base
  leo syntax
  leo check
  leo define-syntax
  leo syntax-case

check syntax? syntax 10

define-syntax
  set-x! stx
  syntax-case
    stx
    when
      set-x! v
      with-implicit
        set-x!: x
        syntax set! x v

define x 10

check equal?: x, 10

set-x! 20

check equal?: x, 20

check free-syntax=?
  with-syntax
    match: foo bar, syntax goo gar
    match: zoo zar, syntax moo mar
    in syntax foo bar zoo zar
  syntax goo gar moo mar

check free-syntax=?
  with-identifier
    foo syntax goo
    bar syntax gar
    in syntax foo bar
  syntax goo gar


check free-syntax=?
  with-identifier*
    foo syntax goo
    bar syntax foo
    in syntax foo bar
  syntax goo goo
