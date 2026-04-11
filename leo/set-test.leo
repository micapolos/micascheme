import
  only: equal?, +, from scheme
  leo define
  leo check
  leo set

define x 10
define y 20

check equal?
  +: x, y
  30

set! x 30
set! y 40

check equal?
  +: x, y
  70
