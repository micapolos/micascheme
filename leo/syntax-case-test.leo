import
  except: define, syntax-case, from scheme
  only: syntax=?, keywords, from micascheme
  leo check
  leo define
  leo syntax-case

check syntax=?
  syntax-case
    syntax magic: "foo"
    keywords: magic
    when
      magic a
      syntax one a
    when
      magic: a, b
      syntax two: a, b
  syntax one "foo"

check syntax=?
  syntax-case
    syntax magic: "foo", "bar"
    keywords: magic
    when
      magic a
      syntax one a
    when
      magic: a, b
      syntax two: a, b
  syntax two: "foo", "bar"

check raises
  syntax-case
    syntax no-magic: "foo", "bar"
    keywords: magic
    when
      magic a
      syntax one a
    when
      magic: a, b
      syntax two: a, b

check syntax=?
  syntax-case
    syntax no-magic: "foo", "bar"
    when
      magic a
      syntax one a
    when
      magic: a, b
      syntax two: a, b
  syntax two: "foo", "bar"

check syntax=?
  syntax-case?
    syntax magic: "foo"
    keywords: magic
    when
      magic a
      syntax one a
  syntax one "foo"

check equal?
  syntax-case?
    syntax magic: "foo", "bar"
    keywords: magic
    when
      magic a
      syntax one a
  #f
