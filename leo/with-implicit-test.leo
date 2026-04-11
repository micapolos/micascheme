import
  leo scheme

define-syntax
  set-x! stx
  syntax-case
    stx
    when
      set-x! v
      with
        implicit: set-x!, x
        syntax set! x v

define x 10

check equal?: x, 10

set-x! 20

check equal?: x, 20
