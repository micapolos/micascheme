comment contains
  native functions
  like: "+", "-", "string-append"

use
  native "#t"
  as
    true
    being boolean
use
  native "#f"
  as
    false
    being boolean
use
  native "not"
  as
    not boolean
    promising boolean
assert true
assert not false

use
  native "="
  as
    number
    equals number
    promising boolean
assert
  2
  equals 2
assert not
  2
  equals 3

use
  native "+"
  as
    number
    plus number
    promising number
assert
  2
  plus 3
  equals 5

use
  native "-"
  as
    number
    minus number
    promising number
assert
  5
  minus 3
  equals 2

use
  native "*"
  as
    number
    times number
    promising number
assert
  2
  times 3
  equals 6

use
  native "/"
  as
    number
    divided by number
    promising number
assert
  6
  divided by 3
  equals 2

use
  native "sin"
  as
    sinus of number
    promising number
assert
  sinus of 0
  equals 0

use
  native "cos"
  as
    cosinus of number
    promising number
assert
  cosinus of 0
  equals 1

use
  native "string=?"
  as
    string
    equals string
    promising boolean
assert
  "foo"
  equals "foo"
assert not
  "foo"
  equals "bar"

use
  native "string-append"
  as
    string
    plus string
    promising string
assert
  "foo"
  plus "bar"
  equals "foobar"

use
  native "string-length"
  as
    string
    offering length number
assert
  "foo"
  get length
  get number
  equals 3

use
  native "number->string"
  as
    number
    offering string
assert
  10
  get string
  equals "10"
