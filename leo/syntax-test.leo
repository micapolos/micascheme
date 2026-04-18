import
  leo base
  leo syntax
  leo check
  leo define-syntax
  leo syntax-case
  leo with
  syntax-keywords

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

check free-syntax=?
  with
    syntax-rules
      when
        foo a
        box a
      when
        foo: a, b
        cons: a, b
    syntax foo 10
  syntax box 10

check free-syntax=?
  with
    syntax-rules
      when
        foo a
        box a
      when
        foo: a, b
        cons: a, b
    syntax foo: 10, 20
  syntax cons: 10, 20

check free-syntax=?
  pattern-id syntax foo
  syntax foo

check free-syntax=?
  pattern-id syntax foo: x, y
  syntax foo

check free-syntax=?
  clause-id syntax when
    foo
    goo gar
  syntax foo

check free-syntax=?
  clause-id syntax when
    foo: x, y
    goo gar
  syntax foo

check free-syntax=?
  rule->clause syntax when
    foo: x, y
    goo gar
  syntax when
    foo: x, y
    syntax goo gar

check free-syntax=?
  rule->clause syntax when
    foo: x, y
    keyword? foo
    goo gar
  syntax when
    foo: x, y
    keyword? foo
    syntax goo gar

check free-syntax=?
  group-clauses syntax :
    when
      foo
      goo gar
    when
      foo: x, y
      goo gar
    when
      bar: x, y, z
      moo mar
  syntax :
    foo
      when
        foo
        goo gar
      when
        foo: x, y
        goo gar
    bar
      when
        bar: x, y, z
        moo mar
