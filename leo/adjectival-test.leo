import
  only
    or, syntax, free-identifier=?
    from scheme
  leo define
  leo lambda
  leo check
  leo adjectival
  syntax

define lambda
  identifier-adjective? x
  or
    free-identifier=?: x, syntax my
    free-identifier=?: x, syntax very
    free-identifier=?: x, syntax lucky

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my number
  syntax my-number

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my lucky number
  syntax my-lucky-number

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my very lucky number
  syntax my-very-lucky-number

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax his number
  syntax his number

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my unlucky number
  syntax my-unlucky number

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my very unlucky number add: 1, 2
  syntax my-very-unlucky number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my number add: 1, 2
  syntax my-number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my lucky number add: 1, 2
  syntax my-lucky-number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my very lucky number add: 1, 2
  syntax my-very-lucky-number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax his number add: 1, 2
  syntax his number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my unlucky number add: 1, 2
  syntax my-unlucky number add: 1, 2

check syntax=?
  transform-adjectival
    identifier-adjective?
    syntax my very unlucky number add: 1, 2
  syntax my-very-unlucky number add: 1, 2

