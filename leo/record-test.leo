import
  except: define, let, from scheme
  leo define
  leo check
  leo record
  leo let
  leo in
  leo make
  leo is?
  leo get
  leo todo
  boolean

define record point: x, y

define record color
  mutable unsigned-8 red
  mutable unsigned-8 green
  mutable unsigned-8 blue

let
  test-point make point: 10, 20
  in
    check equal?: is? point test-point, true
    check equal?: is? color test-point, false
    check equal?: get point x test-point, 10
    check equal?: get point y test-point, 20

let
  test-color make color: 10, 20, 30
  in
    check equal?: is? color test-color, true
    check equal?: is? point test-color, false
    check equal?: get color red test-color, 10
    check equal?: get color green test-color, 20
    check equal?: get color blue test-color, 30

    todo implement
      set! color red: test-color, 255
      check equal?: get color red test-color, 255



